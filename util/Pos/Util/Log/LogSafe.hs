{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Safe/secure logging

module Pos.Util.Log.LogSafe
       ( -- * Logging functions
         SelectiveLogWrapped(..)
       , logMessageS
       , logDebugS
       , logInfoS
       , logNoticeS
       , logWarningS
       , logErrorS
       , logMessageUnsafeP
       , logDebugUnsafeP
       , logInfoUnsafeP
       , logNoticeUnsafeP
       , logWarningUnsafeP
       , logErrorUnsafeP
       , logMessageSP
       , logDebugSP
       , logInfoSP
       , logNoticeSP
       , logWarningSP
       , logErrorSP

         -- * Secure 'Buildable's
       , SecureLog (..)
       , LogSecurityLevel
       , secure
       , unsecure

         -- ** Secure formatters
       , secureF
       , secureMaybeF
       , plainOrSecureF
       , secretOnlyF
       , secureListF
       , buildSafe
       , buildSafeMaybe
       , buildSafeList

         -- ** Secure log utils
       , BuildableSafe
       , BuildableSafeGen (..)
       , SecuredText
       , buildUnsecure
       , getSecuredText
       , deriveSafeBuildable
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)
-- import           Data.List (isSuffixOf)
import           Data.Reflection (Reifies (..), reify)
-- import           Data.Reflection (reify)
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (bprint, build, fconst, later, mapf, (%))
import           Formatting.Internal (Format (..))
import qualified Language.Haskell.TH as TH
import           Pos.Util.Log (CanLog (..), HasLoggerName (..), LogContext, Severity (..),
                               WithLogger, logMessage)

-- import           Pos.Binary.Core ()
import qualified Katip as K

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

-- | Modifies logging so that it writes only to files specified by @s@ type.
newtype SelectiveLogWrapped s m a = SelectiveLogWrapped
    { getSecureLogWrapped :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SelectiveLogWrapped s) where
    lift = SelectiveLogWrapped

data LogPrivacy where
    -- | Only to public logs.
    Public  :: LogPrivacy
    -- | Only to private logs.
    -- Private :: LogPrivacy
    -- -- | To public and private logs.
    -- Both    :: LogPrivacy


-- TODO
-- type LogHandlerTag = Text
-- | Tag identifying handlers.
-- data LogHandlerTag
--     = HandlerFilelike FilePath
--     | HandlerOther String
--     deriving (Show, Eq)

-- | Whether to log to given log handler.
type SelectionMode = LogPrivacy -> Bool

selectPublicLogs :: SelectionMode
selectPublicLogs = const True
-- selectPublicLogs = \case
--     Public -> True
--     _ -> False

selectSecretLogs :: SelectionMode
selectSecretLogs = not . selectPublicLogs

-- TODO
logMCond :: (LogContext m) => Severity -> Text -> (SelectionMode) -> m ()
logMCond sev msg cond = when (cond Public) $ logMessage sev msg

instance (WithLogger m, Reifies s SelectionMode) =>
         CanLog (SelectiveLogWrapped s m) where
    dispatchMessage _ severity msg =
        lift $ logMCond severity msg (reflect (Proxy @s))

instance (K.Katip m) => K.Katip (SelectiveLogWrapped s m) where
    getLogEnv = lift K.getLogEnv

    localLogEnv f a = lift $ K.localLogEnv f $ getSecureLogWrapped a

instance (K.KatipContext m) => K.KatipContext (SelectiveLogWrapped s m) where
  getKatipContext = lift K.getKatipContext

  localKatipContext f a = lift $ K.localKatipContext f $ getSecureLogWrapped a

  getKatipNamespace = lift K.getKatipNamespace

  localKatipNamespace f a = lift $ K.localKatipNamespace f $ getSecureLogWrapped a

instance (HasLoggerName m) => HasLoggerName (SelectiveLogWrapped s m) where
    askLoggerName = SelectiveLogWrapped askLoggerName
    modifyLoggerName foo (SelectiveLogWrapped m) =
        SelectiveLogWrapped (modifyLoggerName foo m)

-- instance (CanLog m) => CanLog (SelectiveLogWrapped s m) where
--     dispatchMessage n s t = lift $ dispatchMessage n s t

execSecureLogWrapped :: Proxy s -> SelectiveLogWrapped s m a -> m a
execSecureLogWrapped _ (SelectiveLogWrapped act) = act

-- | Shortcut for 'logMessage' to use according severity.
logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: (WithLogger m)
    => Text -> m ()
logDebugS   = logMessageS Debug
logInfoS    = logMessageS Info
logNoticeS  = logMessageS Notice
logWarningS = logMessageS Warning
logErrorS   = logMessageS Error

-- | Same as 'logMesssage', but log to secret logs, put only insecure
-- version to memmode (to terminal).
logMessageS
    :: (HasLoggerName m, CanLog m)
    => Severity
    -> Text
    -> m ()
logMessageS severity t =
    reify selectSecretLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- askLoggerName
        dispatchMessage name severity t

----------------------------------------------------------------------------
-- Secure buildables
----------------------------------------------------------------------------

-- * Secutity log levels

-- Stuff below provides way to use different @instance Buildable@ for writing
-- to secret and public logs.
--
-- Example:
--
-- @
-- logInfo $ sformat ("Nyan: "%secureF build) Nyan
-- @
--
-- this function uses @instance Buildable (SecureLog Nyan)@ to display the item.
--
-- @
-- logInfoSP $ \sl -> sformat ("A diamond on the "%buildSafe sl) Rough
-- @
--
-- this function prints value to secret logs as is, while for public logs it uses
-- @instance Buildable (SecureLog Rough)@.
--
-- Logging to public logs only is marked as unsafe, because nothing prevents its
-- user from writting unsecure logs to public.
-- For this reason, if you don't use "security level parameter" on 'logInfoSP' call,
-- remain it as @logInfoSP $ \__sl -> ...@ in order to remember that secured text
-- should be provided inside.

-- Modifies 'instance Buildable' so that it doesn't contain sensitive info
newtype SecureLog a = SecureLog
    { getSecureLog :: a
    } deriving (Eq, Ord)

data LogSecurityLevel
    = SecretLogLevel
    | PublicLogLevel
    deriving (Eq)

secure :: LogSecurityLevel
secure = PublicLogLevel

unsecure :: LogSecurityLevel
unsecure = SecretLogLevel

buildUnsecure :: Buildable a => SecureLog a -> Builder
buildUnsecure (SecureLog a) = bprint build a

type BuildableSafe a = (Buildable a, Buildable (SecureLog a))

-- | Modifies single-parameter formatter to make it use public logging.
secureF :: Format r (SecureLog a -> r) -> Format r (a -> r)
secureF = mapf SecureLog

-- | Secure Maybe using default value, use this to avoid leaking about whether
-- a value is present or not.
secureMaybeF :: a -> Format r (SecureLog a -> r) -> Format r (Maybe a -> r)
secureMaybeF def = mapf (SecureLog . fromMaybe def)

-- | Takes one of given items (usually formatters, nonsecure goes first),
-- depending on security level.
plainOrSecureF :: LogSecurityLevel -> a -> a -> a
plainOrSecureF SecretLogLevel fmt _ = fmt
plainOrSecureF PublicLogLevel _ fmt = fmt

buildSafe :: BuildableSafe a => LogSecurityLevel -> Format r (a -> r)
buildSafe sl = plainOrSecureF sl build (secureF build)

buildSafeMaybe :: BuildableSafe a => a -> LogSecurityLevel -> Format r (Maybe a -> r)
buildSafeMaybe def sl = plainOrSecureF sl build (secureMaybeF def build)

buildSafeList :: BuildableSafe a => LogSecurityLevel -> Format r ([a] -> r)
buildSafeList sl = secureListF sl (secureF build)

-- | Negates single-parameter formatter for public logs.
secretOnlyF :: LogSecurityLevel -> Format r (a -> r) -> Format r (a -> r)
secretOnlyF sl fmt = plainOrSecureF sl fmt (fconst "?")

-- | For public logs hides list content, showing only its size.
-- For secret logs uses provided formatter for list.
secureListF
    :: NontrivialContainer l
    => LogSecurityLevel -> Format r (l -> r) -> Format r (l -> r)
secureListF sl fmt = plainOrSecureF sl fmt lengthFmt
  where
    lengthFmt = later $ \l ->
        if null l
        then "[]"
        else bprint ("[... ("%build%" item(s))]") $ length l

{-
This is helper in generating @instance Buildable a@ and
@instance Buildable (SecureLog a)@ in a single shot.

Sometimes those instances are very similar, and we want to provide
both at once with help of formatter combinators (see functions above).
In such case, define this instance and then use 'deriveSafeBuildable' to
produce the two desired @Buildable@ instances.

You are not supposed to specify this typeclass in constraints, for that
purpose use 'BuildableSafe'.

Example of usage:

@
data Password = Password [Char]

instance BuildableSafeGen Password where
    buildSafeGen sl (Password chars) = bprint (secureListF sl string) chars
    -- "sl" stands for "security level"
@
-}
class BuildableSafeGen a where
    buildSafeGen :: LogSecurityLevel -> a -> Builder

-- | Builds up @instance Buildable a@ and @instance Buildable (SecureLog a)@
-- assuming provided @instance BuildableSafeGen a@.
-- Suitable for simple types.
--
-- Example: @deriveSafeBuildable ''Nyan@
deriveSafeBuildable :: TH.Name -> TH.Q [TH.Dec]
deriveSafeBuildable typeName =
    let typeQ = TH.conT typeName
    in [d|
       instance Buildable $typeQ where
           build = buildSafeGen unsecure

       instance Buildable (SecureLog $typeQ) where
           build = buildSafeGen secure . getSecureLog
       |]

-- | Same as 'logMesssage', put to public logs only (these logs don't go
-- to terminal). Use it along with 'logMessageS' when want to specify
-- secret and public log alternatives manually.
logMessageUnsafeP
    :: (WithLogger m)
    => Severity
    -> Text
    -> m ()
logMessageUnsafeP severity t =
    reify selectPublicLogs $ \s ->
    execSecureLogWrapped s $ do
        name <- askLoggerName
        dispatchMessage name severity t

-- | Shortcut for 'logMessageUnsafeP' to use according severity.
logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: (WithLogger m)
    => Text -> m ()
logDebugUnsafeP   = logMessageUnsafeP Debug
logInfoUnsafeP    = logMessageUnsafeP Info
logNoticeUnsafeP  = logMessageUnsafeP Notice
logWarningUnsafeP = logMessageUnsafeP Warning
logErrorUnsafeP   = logMessageUnsafeP Error


type SecuredText = LogSecurityLevel -> Text

getSecuredText :: LogSecurityLevel -> SecuredText -> Text
getSecuredText = (&)

-- | Same as 'logMesssageSP', put to public and secret logs securely.
logMessageSP
    :: (WithLogger m)
    => Severity -> SecuredText -> m ()
logMessageSP severity securedText = do
    logMessageS severity $ securedText SecretLogLevel
    logMessageUnsafeP severity $ securedText PublicLogLevel

-- | Shortcut for 'logMessage' to use according severity.
logDebugSP, logInfoSP, logNoticeSP, logWarningSP, logErrorSP
    :: (WithLogger m)
    => SecuredText -> m ()
logDebugSP   = logMessageSP Debug
logInfoSP    = logMessageSP Info
logNoticeSP  = logMessageSP Notice
logWarningSP = logMessageSP Warning
logErrorSP   = logMessageSP Error

instance BuildableSafe a => Buildable (SecureLog [a]) where
    build = bprint (buildSafeList secure) . getSecureLog

instance Buildable (SecureLog Text) where
    build _ = "<hidden>"

instance Buildable (SecureLog Word32) where
    build _ = "<bytes>"
