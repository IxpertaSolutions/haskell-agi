module Main
    (main
    )
   where

-- Standard Haskell Modules

import           Control.Monad
import           Control.Monad.Reader
import           Data.Maybe
import           Network

-- 3rd Party Modules

import           Network.AGI
import           Network.AGI.Functions
import           Network.AGI.Type

main :: IO ()
main =
       fastAGI (Just $ PortNumber 4000) mainAGI

mainAGI :: HostName -> PortNumber -> AGI ()
mainAGI _  _ = do
    answer
    var <- liftM agiVars ask
    exec "DIAL" ["SIP/" ++ fromMaybe "Error" (lookup "agi_extension" var)]
    hangUp Nothing
    return ()
