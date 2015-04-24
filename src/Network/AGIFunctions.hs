module Network.AGIFunctions
    ( answer
    , exec
    , getData
    , hangUp
    , record
    , sayDigits
    , sayNumber
    , setMusicOnHold
    , setVariable
    , streamFile
    , waitForDigit
    )
  where

import Network.Type.AGI
import Network.AGI

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Data.Monoid
import Data.Char
import System.IO
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), (<*>), (<$>))

integer = rd <$> (plus <|> minus <|> number)
  where
    rd     = read :: String -> Integer
    plus   = char '+' *> number
    minus  = (:) <$> char '-' <*> number
    number = many1 digit

-- * Result Parsers
parseResult p res =
    case parse p res res of
      (Left e) -> error (show e) -- throwError (userError (show e))
      (Right r) -> r

-- |parse 0 as True, -1 as failure
pSuccessFailure :: CharParser () Bool
pSuccessFailure =
    (char '0' >> return True) <|> (string "-1" >> return False)

-- |parse '200 result='
pResult :: CharParser () String
pResult = string "200 result="

-- |parse a block of zero or more ' ' and '\t' characters (but not '\n')
pSpace :: CharParser () String
pSpace = many (tab <|> char ' ')

pDigitsWithTimeout =
    do digits <- many pDigit
       pSpace
       to <- (string "(timeout)" >> return True) <|> return False
       return (digits, to)

pDigit :: CharParser () Digit
pDigit =
    (char '#' >> return Pound) <|>
    (char '*' >> return Star)  <|>
    (char '0' >> return Zero)  <|>
    (char '1' >> return One)   <|>
    (char '2' >> return Two)   <|>
    (char '3' >> return Three) <|>
    (char '4' >> return Four)  <|>
    (char '5' >> return Five)  <|>
    (char '6' >> return Six)   <|>
    (char '7' >> return Seven) <|>
    (char '8' >> return Eight) <|>
    (char '9' >> return Nine)

pAsciiDigit :: CharParser () Digit
pAsciiDigit =
    do ds <- many1 digit
       case ds of
         "35" -> return Pound
         "42" -> return Star
         "48" -> return Zero
         "49" -> return One
         "50" -> return Two
         "51" -> return Three
         "52" -> return Four
         "53" -> return Five
         "54" -> return Six
         "55" -> return Seven
         "56" -> return Eight
         "57" -> return Nine
         _    -> pzero <?> "The ascii character code " ++ ds ++
                 " (" ++ [chr (read ds)] ++
                 ") does not correspond to a digit on the keypad"

pEndPos :: CharParser () Integer
pEndPos =
    do string "endpos="
       ds <- many1 digit
       return (read ds)


{-
Usage: ANSWER

Answers channel if not already in answer state.

Returns:
failure: 200 result=-1
success: 200 result=0
-}
-- |'answer' channel if not already in answer state
answer :: (MonadIO m) => AGIT m Bool -- ^ True on success, False on failure
answer =
    do res <- sendRecv "ANSWER"
       return $ parseResult (pResult >> pSuccessFailure) res
{-
 Usage: HANGUP [<channelname>]

Hangs up the specified channel.

If no channel name is given, hangs up the current channel.

Returns:
failure: 200 result=-1
success: 200 result=1
-}
-- |hangUp the specified channel
hangUp :: (MonadIO m)
       => Maybe String -- ^ channel to hangup, or current channel if not specified
       -> AGIT m Bool
hangUp mChannel =
    do res <- sendRecv ("HANGUP" ++ maybe "" (' ' :) mChannel)
       return $ parseResult (pResult >> ((char '1' >> return True) <|> (string "-1" >> return False))) res

{-
Usage: GET DATA <file to be streamed> [timeout] [max digits]

Returns:
failure: 200 result=-1
timeout: 200 result=<digits> (timeout)
success: 200 result=<digits>

<digits> is the digits pressed.

-}

-- TODO: does digit include # and * ?
-- |play a file and return and digits pressed
--
-- See also: 'streamFile'
getData :: (MonadIO m)
        => FilePath -- ^ file to stream
        -> Maybe Integer -- ^ timout in ms after keypress (default: 2000 ms)
        -> Maybe Integer -- ^ max
        -> AGIT m (Maybe ([Digit], Bool)) -- ^ Nothing on failure, Just (digits, timeout) on success
getData fp mTimeout mMaxDigits =
    let cmd =
	    "GET DATA " ++ fp ++
                        case (mTimeout, mMaxDigits) of
                          (Nothing, Nothing) -> ""
                          (Just timeout, Nothing) ->  show timeout
                          (Nothing, Just maxDigits) -> " 2000 " ++ show maxDigits
                          (Just timeout, Just maxDigits) -> " " ++ show timeout ++" "++ show maxDigits

    in
      do res <- sendRecv cmd
         return $ parseResult p res
             where
               p = do pResult
                      (try pFail >> return Nothing) <|> liftM Just pDigitsWithTimeout
               pFail = string "-1"
{-
Usage: RECORD FILE <filename> <format> <escape digits> <timeout> [offset samples] [BEEP] [s=<silence>]

Returns:
failure to write: 200 result=-1 (writefile)
failure on waitfor: 200 result=-1 (waitfor) endpos=<offset>
hangup: 200 result=0 (hangup) endpos=<offset>
interrrupted: 200 result=<digit> (dtmf) endpos=<offset>
timeout: 200 result=0 (timeout) endpos=<offset>
random error: 200 result=<error> (randomerror) endpos=<offset>

<offset> is the end offset in the file being recorded.
<digit> is the ascii code for the digit pressed.
<error> ?????
-}

-- |record channel to a file
record :: (MonadIO m)
       => FilePath -- ^ record to this file
       -> SoundType -- ^ |GSM \| WAV|
       -> [Digit] -- ^ stop recording if one of these digits is entered
       -> Maybe Integer -- ^ maximum record time in milliseconds, -1 for no timeout
       -> Maybe Integer -- ^ offset samples
       -> Bool -- ^ beep to indicate recording has begun
       -> Maybe Integer -- ^ stop recording if this many seconds of silence passes
       -> AGIT m (RecordResult, Integer) -- ^ exit condition, endpos=offset
record fp soundType escapeDigits length offset beep silence =
    do res <- sendRecv $ "RECORD FILE " ++ fp ++ " " ++ show soundType ++ " " ++
                          ppEscapeDigits escapeDigits ++ " " ++
                          maybe "-1" show length ++
                          maybe "" (\o -> ' ': show o) offset ++
                          (if beep then " beep" else "") ++
                          maybe "" (\s -> " s=" ++ show s) silence
       return $ parseResult p res

p = pResult >> (pFailureToWrite <|> pFailureOnWaitFor <|> pHangUp <|> pInterrupted <|> pTimeout <|> pRandomError)
    where
      pFailureToWrite =
          try (string "-1 (writefile)") >> return (FailureToWrite, 0)
      pFailureOnWaitFor =
          do try (string "-1 (waitfor)")
             pSpace
             ep <- pEndPos
             return (FailureOnWaitFor, ep)
      pHangUp =
          do try (string "0 (hangup)" <|> string "-1 (hangup)")
             pSpace
             ep <- pEndPos
             return (HangUp, ep)
      pInterrupted = try $
          do digit <- pAsciiDigit
             pSpace
             string "(dtmf)"
             pSpace
             ep <- pEndPos
             return (Interrupted digit, ep)
      pTimeout =
          do try (string "0 (timeout)")
             pSpace
             ep <- pEndPos
             return (Timeout, ep)
      pRandomError = try $
          do error <- manyTill anyChar (try (string " (randomerror)"))
             pSpace
             ep <- pEndPos
             return (RandomError error, ep)

{-
 Usage: SAY DIGITS <number> <escape digits>

Say a given digit string, returning early if any of the given DTMF digits are received on the channel.

EXAMPLE:

SAY DIGITS 5551212 "125#"


The digits five, five, five, one, two, one, two will be spoken out, If durning the speech, the DTMF keys 1, 2, 5 or # are pressed it will stop the playback.

Returns:
failure: 200 result=-1
success: 200 result=0
digit pressed: 200 result=<digit>

<digit> is the ascii code for the digit pressed.
-}

-- |say the given digit string
sayDigits :: (MonadIO m)
          => [Digit] -- ^ digits to say
          -> [Digit] -- ^ digits which can stop playback
          -> AGIT m (Maybe (Maybe Digit)) -- ^ Nothing on error, Just Nothing on success. Just (Just <digit>) if interrupted.
sayDigits digits escapeDigits =
    do res <- sendRecv $ "SAY DIGITS " ++ map ppDigit digits ++ " " ++ ppEscapeDigits escapeDigits
       return $ parseResult p res
    where
      p = do pResult
             (string "-1" >> return Nothing) <|>
               (string "0" >> return (Just Nothing)) <|>
               liftM (Just . Just) pAsciiDigit

{-
SAY NUMBER <number> <escape digits>

Say a given number, returning early if any of the given DTMF digits are received on the channel.

EXAMPLE:

 SAY NUMBER 1234 "1*#"

The number one thousand two hundred and thirty four will be spoken, and if the DTMFs 1, * or # is pressed during the speach it will be terminated.

Returns:
failure: 200 result=-1
success: 200 result=0
digit pressed: 200 result=<digit>

<digit> is the ascii code for the digit pressed.
-}
-- | 'sayNumber' says the specified number
sayNumber :: (MonadIO m)
          => Integer -- ^ number to say
          -> [Digit] -- ^ return early if any of these digits are received
          -> AGIT m (Maybe (Maybe Digit)) -- ^ Nothing on failure, Just Nothing on success, Just (Just <digit>) if key is pressed
sayNumber number escapeDigits =
    do res <- sendRecv ("SAY NUMBER " ++ show number ++ " " ++ ppEscapeDigits escapeDigits)
       return $ parseResult p res
    where
      p = do pResult
             (string "-1" >> return Nothing) <|>
              (string "0" >> return (Just Nothing)) <|>
              liftM (Just . Just) pAsciiDigit

{-
Usage: STREAM FILE <filename> <escape digits> [sample offset]

Send the given file, allowing playback to be interrupted by the given digits, if any.

Use double quotes for the digits if you wish none to be permitted.

If sample offset is provided then the audio will seek to sample offset before play starts.

Remember, the file extension must not be included in the filename.

Returns:
failure: 200 result=-1 endpos=<sample offset>
failure on open: 200 result=0 endpos=0
success: 200 result=0 endpos=<offset>
digit pressed: 200 result=<digit> endpos=<offset>

<offset> is the stream position streaming stopped. If it equals <sample offset> there was probably an error.
<digit> is the ascii code for the digit pressed.

Bugs
STREAM FILE is known to behave inconsistently, especially when used in conjuction with other languages, i.e. Set(LANGUAGE()=xy).
Workaround: Use EXEC PLAYBACK instead.
-}

-- |playback the specified file, can be interupted by the given digits.
--
-- See also: 'getData'
streamFile :: (MonadIO m)
           => FilePath -- ^ file to stream
           -> [Digit] -- ^ escape digits
           -> Maybe Integer -- ^ sample offset
           -> AGIT m (Either Integer (Maybe Digit, Integer)) -- ^ On failure: Left <endpos>. On success: Right (Maybe Digit, <endpos>)
streamFile filePath escapeDigits mSampleOffset =
    do res <- sendRecv $ "STREAM FILE " ++ filePath ++ " " ++ ppEscapeDigits escapeDigits ++ maybe "" (\so -> ' ' :  show so) mSampleOffset
       return $ parseResult p res
    where
      p =
          do pResult
             pFailure <|> pFailureOnOpen <|> pSuccess <|> pSuccessWithDigit
      pFailure =
          do string "-1"
             pSpace
             ep <- pEndPos
             return (Left ep)
      pFailureOnOpen = try $
          do string "0 endpos=0"
             return (Left 0)
      pSuccess = try $
          do string "0"
             pSpace
             ep <- pEndPos
             return (Right (Nothing, ep))
      pSuccessWithDigit = try $
          do d <- pAsciiDigit
             pSpace
             ep <- pEndPos
             return (Right (Just d, ep))

{-
Usage: WAIT FOR DIGIT <timeout>

Waits up to <timeout> milliseconds for channel to receive a DTMF digit.

Use -1 for the <timeout> value if you desire the call to block indefinitely.

Returns:
failure: 200 result=-1
timeout: 200 result=0
success: 200 result=<digit>

<digit> is the ascii code for the digit received.
-}

-- |wait for channel to receive a DTMF digit.
--
-- See also: 'getData' for multiple digits
waitForDigit :: (MonadIO m)
             => Integer -- ^ timeout in milliseconds, -1 to block indefinitely
             -> AGIT m (Maybe (Maybe Digit)) -- ^ |Nothing| on error, |Just Nothing| on timeout, |Just (Just <digit>)| on success
waitForDigit timeout =
    do res <- sendRecv $ "WAIT FOR DIGIT " ++ show timeout
       return $ parseResult p res
  where
    p = do pResult
           (string "-1" >> return Nothing) <|>
             (string "0" >> return (Just Nothing)) <|>
             liftM (Just . Just) pAsciiDigit

{-
Usage: EXEC Dial "IAX2/alice,20"

In some documentations is: EXEC Dial "IAX2/alice|20" which is wrong! Asterisk
it self asks for coma separation.


Executes application with given options.

Returns:
failure: 200 result=-2
success: 200 result=<app_return_code>

<app_return_code> - return code of execute application

-}
exec :: (MonadIO m) => String -> [String] -> AGIT m (Maybe Integer)
exec app []   = return Nothing
exec app args =
    do res <- sendRecv $ "EXEC " ++ app ++ "\"" ++ params ++ "\""
       return $ parseResult p res
  where
    p = do pResult
           (string "-2" >> return Nothing) <|>
             (do retVal <- integer
                 return $ Just retVal)
    params = case args of
                [arg] -> arg
                args  -> mconcat $ map (++ ",") args


{-
Usage: SET CALLERID 12345

This function never fails.


Returns:
success: 200 result=1

-}
-- !!!!!!!!!!!! Change Int into CallerID or something like this
setCallerID :: (MonadIO m) => Int -> AGIT m Bool
setCallerID newCallerID = do
    res <- sendRecv $ "SET CALLERID " ++ show newCallerID
    return $ parseResult p res
  where
    p = do pResult
           (string "1" >> return True) <|>
             (digit >> return False)


{-
Usage: SET VARIABLE foo bar

This function never fails.


Returns:
success: 200 result=1

-}
setVariable :: (MonadIO m) => VariableName -> Variable -> AGIT m Bool
setVariable varName var = do
    res <- sendRecv $ "SET VARIABLE " ++ varName ++ " " ++ var
    return $ parseResult p res
  where
    p = do pResult
           (string "1" >> return True) <|>
             (digit >> return False)

{-
Usage: SET VARIABLE VARIABLENAME VALUE

This function never fails.


Returns:
success: 200 result=0

-}
setMusicOnHold :: (MonadIO m) => OnOff -> MusicOnHoldClass -> AGIT m Bool
setMusicOnHold onOff musicClass = do
    res <- sendRecv $ "SET MUSIC " ++ musicClass ++ " " ++ onOffStr
    return $ parseResult p res
  where
    onOffStr =
        case onOff of
            On ->  "ON"
            Off -> "OFF"
    p = do pResult
           (string "0" >> return True) <|>
             (digit >> return False)
