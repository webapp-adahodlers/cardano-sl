{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , WalletId
  , accountUtxo
  , accountTotalBalance
  , applyBlock
  , applyBlocks
  , bracketPassiveWallet
  , createWalletHdRnd
  , init
  , walletLogMessage
  , walletPassive
  , wallets
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  ) where

import           Universum hiding (State, init)

import           Control.Lens.TH
import           Control.Concurrent.MVar (modifyMVar_, withMVar)
import qualified Data.Map.Strict as Map
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Formatting (sformat, build)

import           System.Wlog (Severity (..))

import           Data.Acid (AcidState)
import           Data.Acid.Memory (openMemoryState)
import           Data.Acid.Advanced (query', update')

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..)
                                                  , prefilterUtxo, prefilterBlock)
import           Cardano.Wallet.Kernel.Types(WalletId (..), WalletESKs)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.DB.AcidState (DB, defDB, dbHdWallets
                                                   , CreateHdWallet (..)
                                                   , ApplyBlock (..)
                                                   , NewPending (..)
                                                   , NewPendingError
                                                   , Snapshot (..))
import           Cardano.Wallet.Kernel.DB.BlockMeta (BlockMeta (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Read as HD
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.DB.Spec.Read as Spec

import           Pos.Core (Timestamp (..), TxAux (..), AddressHash, Coin)

import           Pos.Crypto (EncryptedSecretKey, PublicKey)
import           Pos.Txp (Utxo)
import           Pos.Core.Chrono (OldestFirst)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO () -- ^ Logger
    , _walletESKs       :: MVar WalletESKs           -- ^ ESKs indexed by WalletId
    , _wallets          :: AcidState DB              -- ^ Database handle
    }

makeLenses ''PassiveWallet

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet _walletLogMessage f =
    bracket (liftIO $ openMemoryState defDB)
            (\_ -> return ())
            (\db ->
                bracket
                  (liftIO $ initPassiveWallet _walletLogMessage db)
                  (\_ -> return ())
                  f)

{-------------------------------------------------------------------------------
  Manage the WalletESKs Map
-------------------------------------------------------------------------------}

-- | Insert an ESK, indexed by WalletId, to the WalletESK map
insertWalletESK :: PassiveWallet -> WalletId -> EncryptedSecretKey -> IO ()
insertWalletESK pw wid esk
    = modifyMVar_ (pw ^. walletESKs) (return . f)
    where f = Map.insert wid esk

withWalletESKs :: forall a. PassiveWallet -> (WalletESKs -> IO a) -> IO a
withWalletESKs pw = withMVar (pw ^. walletESKs)

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> AcidState DB
                  -> IO PassiveWallet
initPassiveWallet logMessage db = do
    esks <- Universum.newMVar Map.empty
    return $ PassiveWallet logMessage esks db

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = _walletLogMessage Info "Passive Wallet kernel initialized"

fail' :: (Buildable a, MonadFail m) => a -> m b
fail' e' = fail . toString $ sformat build e'

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates an HD wallet with randomly generated addresses.
--
-- Adds an HdRoot and HdAccounts (which are discovered during prefiltering of utxo).
-- (In the case of empty utxo, no HdAccounts are created.)
--
-- The ESK is indexed by WalletId and added to the WalletESK map.
createWalletHdRnd :: PassiveWallet
                  -> HD.WalletName
                  -> HasSpendingPassword
                  -> AssuranceLevel
                  -> (AddressHash PublicKey, EncryptedSecretKey)
                  -> Utxo
                  -> IO [HdAccountId]
createWalletHdRnd pw@PassiveWallet{..} name spendingPassword assuranceLevel (pk,esk) utxo = do
    created <- InDb <$> getCurrentTimestamp
    res <- update' _wallets
           $ CreateHdWallet rootId name spendingPassword assuranceLevel created utxoByAccount

    -- fails if the HdRootId already exists (see `CreateHdWalletError`)
    either fail' success res
    where
        utxoByAccount = prefilterUtxo rootId esk utxo

        rootId        = HD.HdRootId . InDb $ pk
        walletId      = WalletIdHdRnd rootId
        accountIds    = Map.keys utxoByAccount

        success _arg = insertWalletESK pw walletId esk >> return accountIds

-- TODO find a home for this
-- (NOTE: we are abandoning the 'Mockable time' strategy of the Cardano code base)
getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp . round . (* 1000) <$> getPOSIXTime

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each esk in the `WalletESK` map.
--   Return a unified Map of accountId and prefiltered blocks (representing multiple ESKs)
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO (Map HdAccountId PrefilteredBlock)
prefilterBlock' pw b =
    withWalletESKs pw $ \esks ->
        return
        $ Map.unions
        $ map prefilterBlock_
        $ Map.toList esks
    where
        prefilterBlock_ (wid,esk) = prefilterBlock wid esk b

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        blocksByAccount <- prefilterBlock' pw b
        -- TODO BlockMeta as arg to applyBlock (use checkPoint ^. currentBMeta)
        let blockMeta = BlockMeta . InDb $ Map.empty

        -- apply block to all Accounts in all Wallets
        void $ update' _wallets $ ApplyBlock (blocksByAccount, blockMeta)

-- | Apply multiple blocks, one at a time, to all wallets in the PassiveWallet
--
--   TODO: this will be the responsibility of @matt-noonan's worker thread
applyBlocks :: (Container (f ResolvedBlock))
              => PassiveWallet
              -> OldestFirst f ResolvedBlock
              -> IO ()
applyBlocks pw = mapM_ (applyBlock pw)

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

-- | Submit a new pending transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
newPending :: ActiveWallet -> HdAccountId -> TxAux -> IO (Either NewPendingError ())
newPending ActiveWallet{..} accountId tx
  = update' (walletPassive ^. wallets) $ NewPending accountId (InDb tx)

{-------------------------------------------------------------------------------
  Wallet Account read-only API
-------------------------------------------------------------------------------}

accountUtxo :: PassiveWallet -> HdAccountId -> IO Utxo
accountUtxo pw accountId = do
    db <- query' (pw ^. wallets) Snapshot

    let checkpoint = HD.readHdAccountCurrentCheckpoint accountId (db ^. dbHdWallets)
        utxo'      = Spec.accountUtxo <$> checkpoint

    either unknownAccountErr return utxo'

accountTotalBalance :: PassiveWallet -> HdAccountId -> IO Coin
accountTotalBalance pw accountId = do
    db <- query' (pw ^. wallets) Snapshot

    let checkpoint   = HD.readHdAccountCurrentCheckpoint accountId (db ^. dbHdWallets)
        ourAddrs     = HD.readAddressSetByAccountId      accountId (db ^. dbHdWallets)
        totalBalance = Spec.accountTotalBalance <$> ourAddrs <*> checkpoint

    either unknownAccountErr return totalBalance

unknownAccountErr :: forall a. UnknownHdAccount -> IO a
unknownAccountErr = error . sformat build
