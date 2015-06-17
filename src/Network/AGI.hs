{-# LANGUAGE OverloadedStrings #-}
module Network.AGI
    ( digitsToInteger
    , run
    , fastAGI
    , runInternal
    , sendRecv
    , ppEscapeDigits
    ) where

import           Network.AGI.Type

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception      (finally)
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as LazyText
import           Data.Text.Lazy.Builder
import           Network
import           System.IO
import           System.Posix.Signals


-- |convert a list of 'Digit's into a quoted string.
-- The quoted string format is used by many AGI commands
ppEscapeDigits :: [Digit] -> Text
ppEscapeDigits digits = LazyText.toStrict $ toLazyText $
  singleton '"'
  <> fromString (map ppDigit digits)
  <> singleton '"'

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
run agi hupHandler = do
    _ <- liftIO $ installHandler sigHUP hupHandler Nothing
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
fastAGI portId agi = do
    _ <- installHandler sigPIPE Ignore Nothing
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

readAgiVars :: Handle -> IO (Map Text Text)
readAgiVars h = readAgiVars' M.empty
  where
    readAgiVars' :: Map Text Text -> IO (Map Text Text)
    readAgiVars' m = TIO.hGetLine h >>= \l ->
      return $ case l of
        "" -> m
        _  -> let (k,v) = T.break (':'==) l
              in M.insert k v m

-- |send an AGI Command, and return the Response
--
-- this function provides the low-level send/receive functionality.
sendRecv :: (Applicative m, MonadIO m) => Command -> AGIT m Text
sendRecv cmd = do
    inh  <- agiInH <$> ask
    outh <- agiOutH <$> ask
    liftIO $ do
      _ <- TIO.hPutStrLn inh cmd
      TIO.hGetLine outh
