module Main
    (main
    )
   where

-- Standard Haskell Modules

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad
import Data.Maybe
import Data.Word
import Data.List
import System.IO
import System.Random
import System.Posix.Unistd
import Network

-- 3rd Party Modules

import Network.AGI
import Network.Type.AGI
import Network.AGIFunctions

main :: IO ()
main =
       fastAGI (Just $ PortNumber 4000) mainAGI

mainAGI :: HostName -> PortNumber -> AGI ()
mainAGI _ _= do
    answer
    var <- liftM agiVars ask
    exec "DIAL" ["SIP" ++ fromMaybe "Error" (lookup "agi_extension" var)]
    hangUp Nothing
    return ()
