{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.AGI.Functions
    ( answer
    , exec
    , getData
    , getVariable
    , hangUp
    , record
    , sayDigits
    , sayNumber
    , setMusicOnHold
    , setVariable
    , setCallerID
    , streamFile
    , waitForDigit
    )
  where

import           Network.AGI
import           Network.AGI.Type

import           Control.Applicative (Applicative, (*>), (<$>), (<*), (<*>))
import           Control.Monad.Error
import           Data.Char
import           Data.Foldable       (asum)
import           Data.Functor        ((<$))
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Parsec
import           Text.Parsec.Text    as T

integer :: T.Parser Int
integer = rd <$> (plus <|> minus <|> number)
  where
    rd     = read :: String -> Int
    plus   = char '+' *> number
    minus  = (:) <$> char '-' <*> number
    number = many1 digit

-- * Result Parsers
parseResult :: T.Parser a -> Text -> a
parseResult p res =
    case parse p (T.unpack res) res of
      Left e -> error (show e) -- throwError (userError (show e))
      Right r -> r

-- |parse 0 as True, -1 as failure
pSuccessFailure :: T.Parser Bool
pSuccessFailure = True <$ char '0' <|> False <$ string "-1"

-- |parse '200 result='
pResult :: T.Parser Text
pResult = T.pack <$> string "200 result="

-- | parses value of result as bool
-- | 1 as True, any other digit as False
pBoolResult :: T.Parser Bool
pBoolResult = pResult >> ((True <$ string "1") <|> (False <$ digit))

-- |parse a block of zero or more ' ' and '\t' characters (but not '\n')
pSpace :: T.Parser Text
pSpace = T.pack <$> many (tab <|> char ' ')

pDigitsWithTimeout :: T.Parser ([Digit], Bool)
pDigitsWithTimeout = (,)
    <$> many pDigit
    <*  pSpace
    <*> (True <$ string "(timeout)" <|> return False)

pDigit :: T.Parser Digit
pDigit =
    asum [
        Pound <$ char '#',
        Star <$ char '*',
        Zero <$ char '0',
        One <$ char '1',
        Two <$ char '2',
        Three <$ char '3',
        Four <$ char '4',
        Five <$ char '5',
        Six <$ char '6',
        Seven <$ char '7',
        Eight <$ char '8',
        Nine <$ char '9'
    ]

pAsciiDigit :: T.Parser Digit
pAsciiDigit = do
    ds <- many1 digit
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
        _    -> parserZero <?> concat
                [
                  "The ascii character code ", ds,
                  " (",[chr $ read ds],") ",
                  "does not correspond to a digit on the keypad"
                ]

pEndPos :: T.Parser Integer
pEndPos = read
    <$  string "endpos="
    <*> many1 digit

pMaybeDigit :: T.Parser (Maybe (Maybe Digit))
pMaybeDigit = pResult >> asum
    [
        Nothing <$ string "-1",
        Just Nothing <$ string "0",
        (Just . Just) <$> pAsciiDigit
    ]

{-
Usage: ANSWER

Answers channel if not already in answer state.

Returns:
failure: 200 result=-1
success: 200 result=0
-}
-- |'answer' channel if not already in answer state
answer :: (Applicative m, MonadIO m) => AGIT m Bool -- ^ True on success, False on failure
answer = parseResult (pResult >> pSuccessFailure) <$> sendRecv "ANSWER"
{-
 Usage: HANGUP [<channelname>]

Hangs up the specified channel.

If no channel name is given, hangs up the current channel.

Returns:
failure: 200 result=-1
success: 200 result=1
-}
-- |hangUp the specified channel
hangUp :: (Applicative m, MonadIO m)
       => Maybe Text -- ^ channel to hangup, or current channel if not specified
       -> AGIT m Bool
hangUp mChannel = parseResult pPositiveOne <$> sendRecv (hangup mChannel)
  where
    pPositiveOne :: T.Parser Bool
    pPositiveOne = pResult >> (True <$ char '1' <|> False <$ string "-1")

    hangup :: Maybe Text -> Text
    hangup = mappend "HANGUP" . maybe "" (T.cons ' ')

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
getData :: (Applicative m, MonadIO m)
        => FilePath -- ^ file to stream
        -> Maybe Integer -- ^ timeout in ms after keypress (default: 2000 ms)
        -> Maybe Integer -- ^ max
        -> AGIT m (Maybe ([Digit], Bool)) -- ^ Nothing on failure, Just (digits, timeout) on success
getData fp mTimeout mMaxDigits = parseResult p <$> getData'
  where
    getData' :: (Applicative m, MonadIO m) => AGIT m Text
    getData' = sendRecv $ T.concat
        [
            "GET DATA ", T.pack fp, params mTimeout mMaxDigits
        ]

    params :: Maybe Integer -> Maybe Integer -> Text
    params Nothing Nothing = ""
    params (Just t) Nothing = T.pack $ show t
    params Nothing (Just md) = " 2000 " <> T.pack (show md)
    params (Just t) (Just md) = T.concat
        [
            " ", T.pack $ show t,
            " ", T.pack $ show md
        ]

    p :: T.Parser (Maybe ([Digit], Bool))
    p = pResult >>
        ((Nothing <$ try (string "-1")) <|> (Just <$> pDigitsWithTimeout))

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
record :: (Applicative m, MonadIO m)
       => FilePath -- ^ record to this file
       -> SoundType -- ^ |GSM \| WAV|
       -> [Digit] -- ^ stop recording if one of these digits is entered
       -> Maybe Integer -- ^ maximum record time in milliseconds, -1 for no timeout
       -> Maybe Integer -- ^ offset samples
       -> Bool -- ^ beep to indicate recording has begun
       -> Maybe Integer -- ^ stop recording if this many seconds of silence passes
       -> AGIT m (RecordResult, Integer) -- ^ exit condition, endpos=offset
record fp soundType escapeDigits len offset beep silence = do
    res <- sendRecv $ T.concat
        [
            "RECORD FILE ", T.pack fp, " ", T.pack $ show soundType, " ",
            ppEscapeDigits escapeDigits, " ",
            maybe "-1" (T.pack . show) len,
            maybe "" (T.cons ' ' . T.pack . show) offset,
            if beep then " beep" else "",
            maybe "" (T.append " s=" . T.pack . show)  silence
        ]
    return $ parseResult p res

  where
    p :: T.Parser (RecordResult, Integer)
    p = pResult >> asum
        [
            try pFailureToWrite,
            try pFailureOnWaitFor,
            try pHangUp,
            try pInterrupted,
            try pTimeout,
            try pRandomError
        ]

    pFailureToWrite :: T.Parser (RecordResult, Integer)
    pFailureToWrite = (FailureToWrite, 0) <$ string "-1 (writefile)"

    pFailureOnWaitFor :: T.Parser (RecordResult, Integer)
    pFailureOnWaitFor = (FailureOnWaitFor,)
        <$  string "-1 (waitfor)"
        <*  pSpace
        <*> pEndPos

    pHangUp :: T.Parser (RecordResult, Integer)
    pHangUp = (HangUp,)
        <$ (string "0 (hangup)" <|> string "-1 (hangup)")
        <* pSpace
        <*> pEndPos

    pInterrupted :: T.Parser (RecordResult, Integer)
    pInterrupted = (,) . Interrupted
        <$> pAsciiDigit
        <*  pSpace
        <*  string "(dtmf)"
        <*  pSpace
        <*> pEndPos

    pTimeout :: T.Parser (RecordResult, Integer)
    pTimeout = (Timeout,)
        <$ try (string "0 (timeout)")
        <*  pSpace
        <*> pEndPos

    pRandomError :: T.Parser (RecordResult, Integer)
    pRandomError = (,) . RandomError . T.pack
        <$> manyTill anyChar (try $ string " (randomerror)")
        <*  pSpace
        <*> pEndPos

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
sayDigits :: (Applicative m, MonadIO m)
          => [Digit] -- ^ digits to say
          -> [Digit] -- ^ digits which can stop playback
          -> AGIT m (Maybe (Maybe Digit)) -- ^ Nothing on error, Just Nothing on success. Just (Just <digit>) if interrupted.
sayDigits digits escapeDigits = parseResult pMaybeDigit <$> sayDigits'
    where
      sayDigits' :: (Applicative m, MonadIO m) => AGIT m Text
      sayDigits' = sendRecv $ T.concat
          [
              "SAY DIGITS ",
              T.pack $ map ppDigit digits, " ",
              ppEscapeDigits escapeDigits
          ]

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
sayNumber :: (Applicative m, MonadIO m)
          => Integer -- ^ number to say
          -> [Digit] -- ^ return early if any of these digits are received
          -> AGIT m (Maybe (Maybe Digit)) -- ^ Nothing on failure, Just Nothing on success, Just (Just <digit>) if key is pressed
sayNumber number escapeDigits = parseResult pMaybeDigit <$> sayNumber'
  where
    sayNumber' :: (Applicative m, MonadIO m) => AGIT m Text
    sayNumber' = sendRecv $ T.concat
        [
            "SAY NUMBER ",
            T.pack $ show number, " ",
            ppEscapeDigits escapeDigits
        ]

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
streamFile :: (Applicative m, MonadIO m)
           => FilePath -- ^ file to stream
           -> [Digit] -- ^ escape digits
           -> Maybe Integer -- ^ sample offset
           -> AGIT m (Either Integer (Maybe Digit, Integer)) -- ^ On failure: Left <endpos>. On success: Right (Maybe Digit, <endpos>)
streamFile filePath escapeDigits mSampleOffset = parseResult p <$> fStream
  where
    fStream :: (Applicative m, MonadIO m) => AGIT m Text
    fStream = sendRecv $ T.concat
        [
            "STREAM FILE ",
            T.pack filePath, " ", ppEscapeDigits escapeDigits,
            maybe "" (T.cons ' ' . T.pack . show)  mSampleOffset
        ]

    p :: T.Parser (Either Integer (Maybe Digit, Integer))
    p = pResult >> asum
        [pFailure, try pFailureOnOpen, try pSuccess, try pSuccessWithDigit]

    pFailure  :: T.Parser (Either Integer (Maybe Digit, Integer))
    pFailure = string "-1" >> pSpace >> fmap Left pEndPos

    pFailureOnOpen :: T.Parser (Either Integer (Maybe Digit, Integer))
    pFailureOnOpen = Left 0 <$ string "0 endpos=0"

    pSuccess  :: T.Parser (Either Integer (Maybe Digit, Integer))
    pSuccess = string "0" >> pSpace >> fmap (\ep -> Right (Nothing, ep)) pEndPos

    pSuccessWithDigit  :: T.Parser (Either Integer (Maybe Digit, Integer))
    pSuccessWithDigit = (\dig int -> Right (Just dig, int))
        <$> pAsciiDigit
        <*  pSpace
        <*> pEndPos

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
waitForDigit :: (Applicative m, MonadIO m)
             => Integer -- ^ timeout in milliseconds, -1 to block indefinitely
             -> AGIT m (Maybe (Maybe Digit)) -- ^ |Nothing| on error, |Just Nothing| on timeout, |Just (Just <digit>)| on success
waitForDigit = fmap (parseResult pMaybeDigit) . waitForDigit'
  where
    waitForDigit' :: (Applicative m, MonadIO m) => Integer -> AGIT m Text
    waitForDigit' = sendRecv . mappend "WAIT FOR DIGIT " . T.pack . show

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
exec :: (Applicative m, MonadIO m) => Text -> [Text] -> AGIT m (Maybe Int)
exec app args = parseResult pReturnCode <$> exec'
  where
    exec' :: (Applicative m, MonadIO m) => AGIT m Text
    exec' = sendRecv $ T.concat
        [
            "EXEC ", app,
            concatIfNotEmpty args [" \"", T.intercalate "," args, "\""]
        ]

    concatIfNotEmpty :: [Text] -> [Text] -> Text
    concatIfNotEmpty [] _  = ""
    concatIfNotEmpty _  xs = T.concat xs


    pReturnCode :: T.Parser (Maybe Int)
    pReturnCode = pResult >> ((Nothing <$ string "-2") <|> (Just <$> integer))

{-
Usage: SET CALLERID 12345

This function never fails.


Returns:
success: 200 result=1

-}
-- !!!!!!!!!!!! Change Int into CallerID or something like this
setCallerID :: (Applicative m, MonadIO m) => Int -> AGIT m Bool
setCallerID = fmap (parseResult pBoolResult) . setCID
  where
    setCID ::(Applicative m, MonadIO m) => Int -> AGIT m Text
    setCID = sendRecv . mappend "SET CALLERID " . T.pack . show


{-
Usage: SET VARIABLE foo bar

This function never fails.


Returns:
success: 200 result=1

-}
setVariable :: (Applicative m, MonadIO m) => VariableName -> Variable -> AGIT m Bool
setVariable varName var = parseResult pBoolResult <$> setVar
  where
    setVar ::(Applicative m, MonadIO m) => AGIT m Text
    setVar = sendRecv $ T.concat ["SET VARIABLE ", varName, " ", var]

{-
Usage: SET VARIABLE VARIABLENAME VALUE

This function never fails.


Returns:
success: 200 result=0

-}
setMusicOnHold :: (Applicative m, MonadIO m) => OnOff -> MusicOnHoldClass -> AGIT m Bool
setMusicOnHold onOff musicClass = parseResult pBoolResult <$> setOnHold
  where
    setOnHold :: (Applicative m, MonadIO m) => AGIT m Text
    setOnHold = sendRecv $
      T.concat ["SET MUSIC ", musicClass, " ", onOffStr onOff]

    onOffStr :: OnOff -> Text
    onOffStr On = "ON"
    onOffStr Off = "OFF"

{-
GET VARIABLE UNIQUEID
ANSWER:
200 result=1 (1187188485.0)

Returns:
failure or not set: 200 result=0
success: 200 result=1 <value>
-}
getVariable :: (Applicative m, MonadIO m) => VariableName -> AGIT m (Maybe Variable)
getVariable = fmap (parseResult pMaybeVar) . setVar
  where
    setVar :: (Applicative m, MonadIO m) => Command -> AGIT m Text
    setVar = sendRecv . mappend "GET VARIABLE "

    pMaybeVar :: T.Parser (Maybe Variable)
    pMaybeVar = pResult >> (pZero <|> pVariable)

    pZero :: T.Parser (Maybe Variable)
    pZero = Nothing <$ string "0"

    pVariable :: T.Parser (Maybe Variable)
    pVariable = (Just . T.pack)
        <$  string "1 ("
        <*> many (noneOf ")")
