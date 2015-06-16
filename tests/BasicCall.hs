{-# LANGUAGE OverloadedStrings #-}
module Main
    (
      main
    )
   where

-- Standard Haskell Modules
import           Data.Maybe
import           Data.Monoid
import           Network

-- 3rd Party Modules
import           Network.AGI
import           Network.AGI.Environment
import           Network.AGI.Functions
import           Network.AGI.Type

main :: IO ()
main =
       fastAGI (Just $ PortNumber 4000) mainAGI

mainAGI :: HostName -> PortNumber -> AGI ()
mainAGI _  _ = do
    answer
    ext <- lookupVar "agi_extension"
    exec "DIAL" ["SIP/" <>  fromMaybe "Error" ext]
    hangUp Nothing
    return ()
