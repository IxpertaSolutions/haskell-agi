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
    , asterisVersion
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
    )
  where

import Network.AGI.Type
import Network.AGI

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe


-- | Name of the invoked AGI \"script\".
agiScriptName :: (MonadIO m) => AGIT m FilePath
agiScriptName = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_request" var

-- | Originating channel, that mean channel in which context is running current
-- AGI session.
originatingChannel :: (MonadIO m) => AGIT m String
originatingChannel = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_channel" var

-- | Language code like \"en\".
languageCode :: (MonadIO m) => AGIT m String
languageCode = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_language" var

-- | Originating channel type. It can acquire values: SIP, ZAP ...
originatingChannelType :: (MonadIO m) => AGIT m FilePath
originatingChannelType = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_type" var

-- | Return call unique ID in format \<epoch\>.\<monotonically incremented number\>
-- example: 1165492430.2
--
-- There is some possibility of customising unique ID see:
-- <http://lists.digium.com/pipermail/asterisk-users/2008-March/207108.html>
--
callUniqueID :: (MonadIO m) => AGIT m String
callUniqueID = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_uniqueid" var

asterisVersion :: (MonadIO m) => AGIT m String
asterisVersion = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_version" var

-- | Returns caller id number or \"unknown\".
--
-- TODO: Change return value to Maybe. If \"unknown\" is presented Nothing can
-- be returned for easy and type safe pattern matching.
callerIDNumber :: (MonadIO m) => AGIT m String
callerIDNumber = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_callerid" var


-- | Returns caller id name or \"unknown\".
--
-- TODO: Change return value to Maybe. If "unknown" is presented Nothing can
-- be returned for easy and type safe pattern matching.
callerIDName :: (MonadIO m) => AGIT m String
callerIDName = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_calleridname" var

-- | The presentation for the caller ID in a ZAP channel.
-- @Probably only for ZAP channel.
zapCallingPres :: (MonadIO m) => AGIT m (Maybe String)
zapCallingPres = do
    var <- liftM agiVars ask
    return $ lookup "agi_callingpres" var

-- | The number which is defined in ANI2.
-- PRI Channel only.
callingANI2 :: (MonadIO m) => AGIT m (Maybe String)
callingANI2 = do
    var <- liftM agiVars ask
    return $ lookup "agi_callingani2" var

-- | The type of number used in PRI Channels.
--
-- PRI Channel only.
callingTON :: (MonadIO m) => AGIT m (Maybe String)
callingTON = do
    var <- liftM agiVars ask
    return $ lookup "agi_callington" var

-- | An optional 4 digit number (Transit Network Selector) used in PRI Channels.
--
-- PRI Channel only.
callingTNS :: (MonadIO m) => AGIT m (Maybe String)
callingTNS = do
    var <- liftM agiVars ask
    return $ lookup "agi_callingtns" var

-- | Number which was dialed or \"unknown\". If you want to get dialed
-- extension use extension instead.
--
-- TODO: Change return value to Maybe. If "unknown" is presented Nothing can
-- be returned for easy and type safe pattern matching.
dialedNumber :: (MonadIO m) => AGIT m String
dialedNumber = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_dnid" var
-- | The referring DNIS number or "unknown".
referringDNIS :: (MonadIO m) => AGIT m String
referringDNIS = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_rdnis" var

-- | Originating context from extension.conf.
originatingContext :: (MonadIO m) => AGIT m String
originatingContext = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_context" var

-- | Extension number that is called by user.
extension :: (MonadIO m) => AGIT m String
extension = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_extension" var

-- | The priority it was executed as in the dial plan.
execPriority :: (MonadIO m) => AGIT m String
execPriority = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_priority" var

-- | Check if AGI connection is enhanced AGI version (EAGI).
isEnhanced :: (MonadIO m) => AGIT m Bool
isEnhanced = do
    var <- liftM agiVars ask
    case fromJust $ lookup "agi_enhanced" var of
        "0.0" -> return False
        "1.0" -> return True
        _     -> return False

-- | Account code of the origin channel.
accountCode :: (MonadIO m) => AGIT m String
accountCode = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_accountcode" var

-- | Thread ID of the AGI script.
threadID :: (MonadIO m) => AGIT m String
threadID = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_threadid" var

-- | Get arguments passed by AGI call form extension.conf.
arg :: (MonadIO m) => Int -> AGIT m (Maybe String)
arg argNum = do
    var <- liftM agiVars ask
    return $ lookup ("agi_arg_" ++ show argNum) var

