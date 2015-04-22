module Network.AGI
    ( digitsToInteger
    , run
    , fastAGI
    , runInternal
    , sendRecv
    , ppEscapeDigits
    ) where

import Network.Type.AGI

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad.Reader
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Word
import Network
import Text.ParserCombinators.Parsec
import System.IO
import System.Posix.Signals
import System.Random


-- |convert a list of 'Digit's into a quoted string.
-- The quoted string format is used by many AGI commands
ppEscapeDigits :: [Digit] -> String
ppEscapeDigits digits = '"' : (map ppDigit digits  ++ "\"")

-- |convert a list of 'Digit's to an 'Integer'.
-- Will fail if the list is empty or contains * or #
digitsToInteger :: [Digit] -> Maybe Integer
digitsToInteger digits =
    case reads (map ppDigit digits) of
      [(i, [])] -> Just i
      _ -> Nothing

-- TODO: let user install a custom sipHUP handler (sigHUP is sent when the caller hangs ups)
-- |Top-level wrapper for single-shot AGI scripts.
--
-- Example:
--
-- @ main = run yourAGI Ignore @
run :: (MonadIO m) => AGIT m a -> Handler -> m a
run agi hupHandler =
    do liftIO $ installHandler sigHUP hupHandler Nothing
       runInternal agi stdin stdout

-- |Top-level for long running AGI scripts.
--
-- Example:
--
-- @ main = fastAGI Nothing yourAGI @
--
-- You should be sure to compile with -threaded. Note that 'yourAGI'
-- may be running simultaneously in multiple threads, so you will need
-- some concurrency control for shared data.
--
-- TODO: support a hang-up handler
-- TODO: ability to listen on a specific IP address
fastAGI :: Maybe PortID -> (HostName -> PortNumber -> AGI a) -> IO ()
fastAGI portId agi =
    do installHandler sigPIPE Ignore Nothing
       s <- listenOn $ fromMaybe (PortNumber 4573) portId
       forever
          (do (h, hostname, portNum) <- accept s
              forkIO $ runInternal (agi hostname portNum) h h >> hClose h)
          `finally` sClose s



-- |runInternal - run an AGI script using the supplied Handles for input and output
--
-- You probably want 'run' or 'fastAGI'. This function is exposed so
-- that 3rd party libraries such as HAppS can easily add support for
-- FastAGI support.
--
-- TODO: support general method of handling extra arguments (query_string vs command-line arguments)
runInternal :: (MonadIO m) => AGIT m a -> Handle -> Handle -> m a
runInternal agi inh outh =
    do vars <- liftIO $ readAgiVars inh
       liftIO $ hSetBuffering inh  LineBuffering
       liftIO $ hSetBuffering outh LineBuffering
       runReaderT (runAGIT agi) (AGIEnv vars inh outh)

readAgiVars :: Handle -> IO [(String, String)]
readAgiVars inh =
    do mAgiVar <- readAgiVar
       case mAgiVar of
	    Nothing ->
		return []
	    Just agiVar ->
		do rest <- readAgiVars inh
		   return (agiVar:rest)
    where readAgiVar :: IO (Maybe (String, String))
	  readAgiVar =
	      do l <- hGetLine inh
		 case l of
		      "" -> return Nothing
		      _ -> let (a,v) = break (':' ==) l in
				       return (Just (a, dropWhile (' ' ==) (tail v)))

-- |send an AGI Command, and return the Response
--
-- this function provides the low-level send/receive functionality.
sendRecv :: (MonadIO m) => Command -> AGIT m String
sendRecv cmd =
    do inh  <- liftM agiInH ask
       outh <- liftM agiOutH ask
       liftIO $ do hPutStrLn inh cmd
                   hGetLine outh
