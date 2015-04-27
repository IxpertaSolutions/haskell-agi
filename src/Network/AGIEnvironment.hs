module Network.AGIEnvironment
    ( envGetRequest
    , envGetChannel
    , envGetLanguage
    , envGetType
    , envGetUniqueID
    , envGetVersion
    , envGetCallerID
    , envGetCallerIDName
    , envGetCallingpres
    , envGetCallingANI2
    , envGetCallingTON
    , envGetCallingTNS
    , envGetDNID
    , envGetRDNIS
    , envGetContext
    , envGetExtension
    , envGetPriority
    , envIsEnhanced
    , envGetAccountCode
    , envGetThreadID
    , envGetArg
    )
  where

import Network.Type.AGI
import Network.AGI

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe

envGetRequest :: (MonadIO m) => AGIT m FilePath
envGetRequest = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_request" var

envGetChannel :: (MonadIO m) => AGIT m String
envGetChannel = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_channel" var

envGetLanguage :: (MonadIO m) => AGIT m String
envGetLanguage = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_language" var

envGetType :: (MonadIO m) => AGIT m FilePath
envGetType = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_type" var

envGetUniqueID :: (MonadIO m) => AGIT m String
envGetUniqueID = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_uniqueid" var

envGetVersion :: (MonadIO m) => AGIT m String
envGetVersion = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_version" var

envGetCallerID :: (MonadIO m) => AGIT m String
envGetCallerID = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_callerid" var

envGetCallerIDName :: (MonadIO m) => AGIT m String
envGetCallerIDName = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_calleridname" var

envGetCallingpres :: (MonadIO m) => AGIT m (Maybe String)
envGetCallingpres = do
    var <- liftM agiVars ask
    return $ lookup "agi_callingpres" var

envGetCallingANI2 :: (MonadIO m) => AGIT m (Maybe String)
envGetCallingANI2 = do
    var <- liftM agiVars ask
    return $ lookup "agi_callingani2" var

envGetCallingTON :: (MonadIO m) => AGIT m (Maybe String)
envGetCallingTON = do
    var <- liftM agiVars ask
    return $ lookup "agi_callington" var

envGetCallingTNS :: (MonadIO m) => AGIT m (Maybe String)
envGetCallingTNS = do
    var <- liftM agiVars ask
    return $ lookup "agi_callingtns" var

envGetDNID :: (MonadIO m) => AGIT m String
envGetDNID = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_dnid" var

envGetRDNIS :: (MonadIO m) => AGIT m String
envGetRDNIS = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_rdnis" var

envGetContext :: (MonadIO m) => AGIT m String
envGetContext = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_context" var

envGetExtension :: (MonadIO m) => AGIT m String
envGetExtension = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_extension" var

envGetPriority :: (MonadIO m) => AGIT m String
envGetPriority = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_priority" var

envIsEnhanced :: (MonadIO m) => AGIT m Bool
envIsEnhanced = do
    var <- liftM agiVars ask
    case fromJust $ lookup "agi_enhanced" var of
        "0.0" -> return False
        "1.1" -> return True
        _     -> return False

envGetAccountCode :: (MonadIO m) => AGIT m String
envGetAccountCode = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_accountcode" var

envGetThreadID :: (MonadIO m) => AGIT m String
envGetThreadID = do
    var <- liftM agiVars ask
    return $ fromJust $ lookup "agi_threadid" var

envGetArg :: (MonadIO m) => Int -> AGIT m (Maybe String)
envGetArg argNum = do
    var <- liftM agiVars ask
    return $ lookup ("agi_arg_" ++ show argNum) var

