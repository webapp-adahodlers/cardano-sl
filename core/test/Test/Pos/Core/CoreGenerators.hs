module Test.Pos.Core.CoreGenerators
       ( gen32bytes,
         genBool,
         genCoin,
         genKeyPair,
         genPkWit,
         genProtoMag,
         genPubKey,
         genPubKeyAddr,
         genRedKeyPair,
         genRedPubKey,
         genRedSecKey,
         genRedSig,
         genRedWit,
         genScript,
         genScriptVersion,
         genScriptWit,
         genSecKey,
         genSignTag,
         genTx,
         genTxAttributes,
         genTxHash,
         genTxId,
         genTxIn,
         genTxIndex,
         genTxInList,
         genTxOut,
         genTxOutList,
         genTxSig,
         genTxSigData,
         genUnkWit,
         pkWitness,
         redWitness,
         scrWitness,
         unkWitness,
       ) where

import           Universum

import           Cardano.Crypto.Wallet (xsignature)
import           Data.Maybe (fromJust)
import           Data.Text
import           Hedgehog (Gen)
import           Pos.Binary.Core()
import           Pos.Core.Common (Address (..), Coin (..), IsBootstrapEraAddr (..)
                                 , makePubKeyAddress, Script (..), ScriptVersion)
import           Pos.Core.Txp ( TxAttributes, Tx (..), TxId, TxIn (..), TxInWitness (..)
                              , TxOut (..), TxSig , TxSigData (..))
import           Pos.Crypto.Configuration
import           Pos.Crypto.Hashing ( Hash, unsafeHash)
import           Pos.Crypto.Signing ( deterministicKeyGen, parseFullPublicKey, PublicKey(..)
                                    , redeemDeterministicKeyGen, RedeemPublicKey(..)
                                    , RedeemSecretKey(..), redeemSign, RedeemSignature(..), sign
                                    , SecretKey(..), Signature (..), SignTag(..))
import           Pos.Data.Attributes (mkAttributes)
import           Prelude

import qualified Crypto.Sign.Ed25519 as Ed25519
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range



--------------- Hedgehog Core Generators ---------------


gen32bytes :: Gen ByteString
gen32bytes = Gen.bytes (Range.singleton 32)

genBool :: Gen Bool
genBool = Gen.bool

genCoin :: Gen Coin
genCoin = Coin <$> Gen.word64 Range.constantBounded

genKeyPair :: Gen (PublicKey, SecretKey)
genKeyPair = deterministicKeyGen <$> gen32bytes

genPkWit :: Gen TxInWitness
genPkWit = PkWitness <$> genPubKey <*> genTxSig

genProtoMag :: Gen ProtocolMagic
genProtoMag = ProtocolMagic <$> Gen.int32 Range.constantBounded

genPubKey :: Gen PublicKey
genPubKey = fst <$> genKeyPair -- Generates `PublicKey` with  empty password.

genPubKeyAddr :: Gen Address
genPubKeyAddr = makePubKeyAddress <$> (IsBootstrapEraAddr <$> genBool) <*> genPubKey

genRedKeyPair :: Gen (RedeemPublicKey, RedeemSecretKey)
genRedKeyPair = fromJust <$> (redeemDeterministicKeyGen <$> gen32bytes)

genRedPubKey :: Gen RedeemPublicKey
genRedPubKey = fst <$> genRedKeyPair

genRedSecKey :: Gen RedeemSecretKey
genRedSecKey = snd <$> genRedKeyPair

genRedSig :: Gen (RedeemSignature TxSigData)
genRedSig = redeemSign <$> genProtoMag  <*> genSignTag <*> genRedSecKey <*> genTxSigData

genRedWit :: Gen TxInWitness
genRedWit = RedeemWitness <$> genRedPubKey <*> genRedSig

genScript :: Gen Script
genScript = Script <$> genScriptVersion <*> gen32bytes

genScriptVersion :: Gen ScriptVersion
genScriptVersion = Gen.word16 Range.constantBounded

genScriptWit :: Gen TxInWitness
genScriptWit = ScriptWitness <$> genScript <*> genScript

genSecKey :: Gen SecretKey
genSecKey = snd <$> genKeyPair

genSignTag :: Gen SignTag
genSignTag =  Gen.element [ SignForTestingOnly
                          , SignTx
                          , SignRedeemTx
                          , SignVssCert
                          , SignUSProposal
                          , SignCommitment
                          , SignUSVote
                          , SignMainBlock
                          , SignMainBlockLight
                          , SignMainBlockHeavy
                          , SignProxySK
                          ]


genTx :: Gen Tx
genTx = UnsafeTx <$> genTxInList <*> genTxOutList <*> genTxAttributes

genTxAttributes :: Gen TxAttributes
genTxAttributes = return $ mkAttributes ()

genTxHash :: Gen (Hash Tx)
genTxHash = unsafeHash <$> genTx

genTxId :: Gen TxId
genTxId = unsafeHash <$> genPubKey

genTxIn :: Gen TxIn
genTxIn = TxInUtxo <$> genTxId <*> genTxIndex

genTxIndex :: Gen Word32
genTxIndex = Gen.word32 (Range.constant 1 10)

genTxInList :: Gen (NonEmpty TxIn)
genTxInList = Gen.nonEmpty (Range.constant 1 10) genTxIn

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genPubKeyAddr <*> genCoin

genTxOutList :: Gen (NonEmpty TxOut)
genTxOutList = Gen.nonEmpty (Range.constant 1 10) genTxOut

genTxSig :: Gen TxSig
genTxSig =  sign <$> genProtoMag <*> genSignTag <*> genSecKey <*> genTxSigData

genTxSigData :: Gen TxSigData
genTxSigData = TxSigData <$> genTxHash

genUnkWit :: Gen TxInWitness
genUnkWit = UnknownWitnessType <$> Gen.word8 Range.constantBounded <*> Gen.bytes (Range.constant 0 50)


------------------------- Golden Test Data -----------------------------

-- | PkWitness

seedBS :: ByteString
seedBS = "nvpbkjoxpmemkgicmqxdixdwrsbhwdtqxhtzipzfwkjzzkwifcgszbqmfjyrxrrg"

-- | `XSignature` constructors are not exported from cardano-crypto.
-- Therefore `TxSig` must be constructed with `xsignature`.

txSig :: TxSig
txSig = case xsignature seedBS of
            Right xsig -> Signature xsig
            Left err -> Prelude.error $ "txSig error:" ++ err

-- | `seedPubK` generated with `formatFullPublicKey` and `genPubKey`.

seedPubK :: Text
seedPubK = "eFZDwMJmAX3KKaRV/zLx2PabIE3un79kQ36oZuKFUMjA6wW5zsmOmvRwWoL8gMfw\
           \+xNz1bSHAZvRt47V+iTb7g=="

pubKey :: PublicKey
pubKey = case parseFullPublicKey seedPubK of
            Right pk -> pk
            Left err -> Universum.error (toText ("Seed public key failed to parse: " :: Text) `Data.Text.append` err)

pkWitness :: TxInWitness
pkWitness = PkWitness pubKey txSig

-- | ScriptWitness

scrWitness :: TxInWitness
scrWitness = ScriptWitness (Script 52147 "ootteiijvizitypuleyshqtoihcncwia") (Script 732 "xkuiffugzccekevklqdcvjrkrwrxjwdg")

-- | RedeemWitness

redWitPubKey :: RedeemPublicKey
redWitPubKey = RedeemPublicKey $ Ed25519.PublicKey "cnrzviaomratmiowjsrqpjmhlfwnlrnu"

redWitSig :: RedeemSignature TxSigData
redWitSig = RedeemSignature (Ed25519.Signature "34r\GSg\129\DC3\209\228\245]\170\189\158\161\22 \
                                                \ 9\t\150'\249\EM$\226q\225\DC1?\136\194\171\15 \
                                                \ 7\167\136t\248\ff\167\203\222\224\142=?\DC34$ \
                                                \z\228Ev\250\161:\a\148\SO\175\233\239\150\t\CAN\ETX")

redWitness :: TxInWitness
redWitness =  RedeemWitness redWitPubKey redWitSig

-- | UnknownTypeWitness

unkWitness :: TxInWitness
unkWitness = UnknownWitnessType 127 "\133\SYNE\STX"
