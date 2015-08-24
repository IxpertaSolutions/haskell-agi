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
import           Control.Exception
    ( bracketOnError
    , catch
    , finally
    , IOException
    )
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Monoid
import           Data.Char
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as LazyText
import           Data.Text.Lazy.Builder

import           Network.Socket
    ( AddrInfo (..)
    , AddrInfoFlag (..)
    , bind
    , close
    , defaultHints
    , defaultProtocol
    , Family (..)
    , getAddrInfo
    , HostName
    , listen
    , maxListenQueue
    , ServiceName
    , socket
    , SocketType (..)
    )
import           Network (accept)

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
-- If Socket Address is not provided the fastAGI will listen on all ports and
-- the Default port (4573) will be used.
--
-- You should be sure to compile with -threaded. Note that 'yourAGI'
-- may be running simultaneously in multiple threads, so you will need
-- some concurrency control for shared data.
--
-- TODO: support a hang-up handler
fastAGI :: Maybe HostName -> Maybe ServiceName -> AGI a -> IO ()
fastAGI addr port agi = do
    _ <- installHandler sigPIPE Ignore Nothing
    addr' <- getAddrInfo (Just hints) justHost justPort
    tryAddrs addr'
  where
    hints = defaultHints { addrFlags = [AI_PASSIVE]
                         , addrSocketType = Stream
                         }
    justHost = Just $ fromMaybe "localhost" addr
    justPort = Just $ fromMaybe "4573" port

    tryAddrs []     = error "bindSock: no addresses available"
    tryAddrs [x]    = useAddr x
    tryAddrs (x:xs) = catch (useAddr x)
                            (\e -> let _ = e :: IOException in tryAddrs xs)

    useAddr addr'' =
        bracketOnError
            (socket AF_INET Stream defaultProtocol)
            (close)
            (\sock -> do
                bind sock $ addrAddress addr''
                listen sock maxListenQueue
                forever
                    (do (h, _, _) <- accept sock
                        forkIO $ runInternal agi h h >> hClose h)
                    `finally` close sock
            )



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
      case l of
          "" -> return $ m
          _  -> let (k,v) = T.break (':'==) l
                in readAgiVars' $ M.insert k (T.dropWhile isGarbage v) m
    isGarbage x = isSpace x || x == ':'

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
