{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.AGI.Environment
Copyright   : (c) Jan Šipr, 2015
License     : BSD3
Maintainer  : sipr.jan@gmail.com
Stability   : experimental
Portability : POSIX

When asterisk dial-plan executes AGI script it passes some initial variables for
AGI script usage.
-}

module Network.AGI.Environment
    (
-- * AGI Environment getters
      agiScriptName
    , originatingChannel
    , languageCode
    , originatingChannelType
    , callUniqueID
    , asteriskVersion
    , callerIDNumber
    , callerIDName
    , zapCallingPres
    , callingANI2
    , callingTON
    , callingTNS
    , dialedNumber
    , referringDNIS
    , originatingContext
    , extension
    , execPriority
    , isEnhanced
    , accountCode
    , threadID
    , arg
    , lookupVar
    , getVarWith
    )
  where

import           Network.AGI.Type

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy             as LazyText
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int


-- | Name of the invoked AGI \"script\".
agiScriptName :: (Applicative m, Monad m) => AGIT m Text
agiScriptName = fromJust <$> lookupVar "agi_request"

-- | Originating channel, that mean channel in which context is running current
-- AGI session.
originatingChannel :: (Applicative m, Monad m) => AGIT m Text
originatingChannel = fromJust <$> lookupVar "agi_channel"

-- | Language code like \"en\".
languageCode :: (Applicative m, Monad m) => AGIT m Text
languageCode = fromJust <$> lookupVar "agi_language"

-- | Originating channel type. It can acquire values: SIP, ZAP ...
originatingChannelType :: (Applicative m, Monad m) => AGIT m Text
originatingChannelType = fromJust <$> lookupVar "agi_type"

-- | Return call unique ID in format \<epoch\>.\<monotonically incremented number\>
-- example: 1165492430.2
--
-- There is some possibility of customising unique ID see:
-- <http://lists.digium.com/pipermail/asterisk-users/2008-March/207108.html>
--
callUniqueID :: (Applicative m, Monad m) => AGIT m Text
callUniqueID = fromJust <$> lookupVar  "agi_uniqueid"

asteriskVersion :: (Applicative m, Monad m) => AGIT m Text
asteriskVersion = fromJust <$> lookupVar "agi_version"

-- | Returns caller id number or \"unknown\".
--
-- TODO: Change return value to Maybe. If \"unknown\" is presented Nothing can
-- be returned for easy and type safe pattern matching.
callerIDNumber :: (Applicative m, Monad m) => AGIT m Text
callerIDNumber = fromJust <$> lookupVar "agi_callerid"


-- | Returns caller id name or \"unknown\".
--
-- TODO: Change return value to Maybe. If "unknown" is presented Nothing can
-- be returned for easy and type safe pattern matching.
callerIDName :: (Applicative m, Monad m) => AGIT m Text
callerIDName = fromJust <$> lookupVar "agi_calleridname"

-- | The presentation for the caller ID in a ZAP channel.
-- Probably only for ZAP channel.
zapCallingPres :: (Monad m) => AGIT m (Maybe Text)
zapCallingPres = lookupVar "agi_callingpres"

-- | The number which is defined in ANI2.
-- PRI Channel only.
callingANI2 :: (Monad m) => AGIT m (Maybe Text)
callingANI2 = lookupVar "agi_callingani2"

-- | The type of number used in PRI Channels.
--
-- PRI Channel only.
callingTON :: (Monad m) => AGIT m (Maybe Text)
callingTON = lookupVar "agi_callington"

-- | An optional 4 digit number (Transit Network Selector) used in PRI Channels.
--
-- PRI Channel only.
callingTNS :: (Monad m) => AGIT m (Maybe Text)
callingTNS = lookupVar "agi_callingtns"

-- | Number which was dialed or \"unknown\". If you want to get dialed
-- extension use extension instead.
--
-- TODO: Change return value to Maybe. If "unknown" is presented Nothing can
-- be returned for easy and type safe pattern matching.
dialedNumber :: (Applicative m, Monad m) => AGIT m Text
dialedNumber = fromJust <$> lookupVar "agi_dnid"
-- | The referring DNIS number or "unknown".
referringDNIS :: (Applicative m, Monad m) => AGIT m Text
referringDNIS = fromJust <$> lookupVar "agi_rdnis"

-- | Originating context from extension.conf.
originatingContext :: (Applicative m, Monad m) => AGIT m Text
originatingContext = fromJust <$> lookupVar "agi_context"

-- | Extension number that is called by user.
extension :: (Applicative m, Monad m) => AGIT m Text
extension = fromJust <$> lookupVar "agi_extension"

-- | The priority it was executed as in the dial plan.
execPriority :: (Applicative m, Monad m) => AGIT m Text
execPriority = fromJust <$> lookupVar "agi_priority"

-- | Check if AGI connection is enhanced AGI version (EAGI).
isEnhanced :: (Monad m) => AGIT m Bool
isEnhanced = lookupVar "agi_enhanced" >>= \v ->
  case v of
    Just "1.0" -> return True
    _          -> return False

-- | Account code of the origin channel.
accountCode :: (Applicative m, Monad m) => AGIT m Text
accountCode = fromJust <$> lookupVar "agi_accountcode"

-- | Thread ID of the AGI script.
threadID :: (Applicative m, Monad m) => AGIT m Text
threadID = fromJust <$> lookupVar "agi_threadid"

-- | Get arguments passed by AGI call form extension.conf.
arg :: (Monad m) => Int -> AGIT m (Maybe Text)
arg argNum = lookupVar name
  where name = LazyText.toStrict . toLazyText $ "agi_arg_" <> decimal argNum

getVarWith :: (Monad m) => (Map Text Text -> a) -> AGIT m a
getVarWith f = asks $ f . agiVars

lookupVar :: (Monad m) => Text -> AGIT m (Maybe Text)
lookupVar name = getVarWith $ M.lookup name
