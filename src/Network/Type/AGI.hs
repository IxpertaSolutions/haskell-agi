{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Network.Type.AGI
    ( AGI
    , AGIEnv(..)
    , AGIT(..)
    , Command
    , Digit(..)
    , ppDigit
    , RecordResult(..)
    , SoundType(..)
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Data.Generics
import System.Posix.Signals
import System.IO


data AGIEnv = AGIEnv { agiVars :: [(String, String)]
                     , agiInH :: Handle
                     , agiOutH :: Handle
                     }

newtype AGIT m a = AGI { runAGIT :: ReaderT AGIEnv m a }
    deriving (Monad, MonadIO, Functor, MonadReader AGIEnv)

type AGI = AGIT IO

-- |DTMF digits
data Digit
    = Pound
    | Star
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
      deriving (Eq, Ord, Read, Show, Enum, Data, Typeable)

-- |convert a 'Digit' to its ASCII representation
ppDigit :: Digit -> Char
ppDigit Pound = '#'
ppDigit Star  = '*'
ppDigit Zero  = '0'
ppDigit One   = '1'
ppDigit Two   = '2'
ppDigit Three = '3'
ppDigit Four  = '4'
ppDigit Five  = '5'
ppDigit Six   = '6'
ppDigit Seven = '7'
ppDigit Eight = '8'
ppDigit Nine  = '9'

type Command = String
-- data Timeout =  Timeout Word (Maybe Word) -- ^ timeout, max digits

data SoundType = WAV | GSM
               deriving (Eq, Enum, Data, Typeable)

instance Show SoundType where
    show WAV = "wav"
    show GSM = "gsm"


data RecordResult
    = FailureToWrite
    | FailureOnWaitFor
    | HangUp
    | Interrupted Digit
    | Timeout
    | RandomError String
      deriving (Eq, Show, Data, Typeable)

