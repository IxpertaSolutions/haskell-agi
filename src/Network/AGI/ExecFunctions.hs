{-# LANGUAGE OverloadedStrings #-}

module Network.AGI.ExecFunctions
    ( execDumpChan
    , setMusicOnHold
    )
  where

import Data.Monoid ((<>))

import Control.Applicative (Applicative)
import Control.Monad.Error

import Network.AGI.Functions
import Network.AGI.Type

execDumpChan :: (Applicative m, MonadIO m) => AGIT m ()
execDumpChan = exec "DUMPCHAN" [] >> return ()

{-
Usage: Set music on hold class for current call. Music on hold class is set
(play list) of music played id the call is set on hold.

AGI call SET MUSIC ON HOLD is deprecated and it is not working from some reason
so exec is used just to be sure ;)

This function never fails.

-}
setMusicOnHold :: (Applicative m, MonadIO m) => MusicOnHoldClass -> AGIT m ()
setMusicOnHold musicClass =
    void $ exec "SET" ["CHANNEL(musicclass)="<>musicClass]

