{-# LANGUAGE OverloadedStrings #-}
module Main
    (
      main
    )
   where

-- Standard Haskell Modules
import           Data.Maybe
import           Data.Monoid

-- 3rd Party Modules
import           Network.AGI
import           Network.AGI.Environment
import           Network.AGI.Functions
import           Network.AGI.Type

main :: IO ()
main =
       fastAGI Nothing Nothing mainAGI

mainAGI :: AGI ()
mainAGI = do
    _  <- answer
    ext <- lookupVar "agi_extension"
    _ <- exec "DIAL" ["SIP/" <>  fromMaybe "Error" ext]
    _ <- hangUp Nothing
    return ()
