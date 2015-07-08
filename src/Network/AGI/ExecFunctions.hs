{-# LANGUAGE OverloadedStrings #-}

module Network.AGI.ExecFunctions
    ( execDumpChan
    )
  where

import           Control.Applicative (Applicative)
import           Control.Monad.Error

import           Network.AGI.Functions
import           Network.AGI.Type

execDumpChan :: (Applicative m, MonadIO m) => AGIT m ()
execDumpChan = exec "DUMPCHAN" [] >> return ()
