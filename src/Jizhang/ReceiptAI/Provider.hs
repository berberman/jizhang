{-# LANGUAGE OverloadedStrings #-}

module Jizhang.ReceiptAI.Provider
  ( ReceiptAIOutcome (..),
    ReceiptAIUsage (..),
    ReceiptAIAttempt (..),
    ReceiptAIReadError (..),
    buildReceiptAIRequest,
    readReceiptAIResponse,
    decodeReceiptAIResponse,
    decodeExtractionOutput,
    extractReceipt,
    extractReceiptWithDeadline,
    estimateReceiptAICost,
    receiptAIPricingVersion,
  )
where

import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, tryJust)
import Control.Monad (guard, unless)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Parser (jsonNoDup, jstring)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTimeNSec)
import Jizhang.API.Types.ReceiptAI (ReceiptEvidence, ReceiptExtractionOutput (..), ReceiptImageInput (..), maxReceiptEvidenceBytes)
import Jizhang.ReceiptAI.Contract (ReceiptAIContract (..))
import Jizhang.ReceiptAI.Image (ValidatedReceiptImage, validatedImageInput)
import Jizhang.ReceiptAI.Runtime (ReceiptAIKey, receiptAIKeyText)
import Network.HTTP.Client
  ( BodyReader,
    HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request (..),
    RequestBody (..),
    brRead,
    defaultRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    responseTimeoutMicro,
    withResponse,
  )
import Network.HTTP.Types.Status (statusCode)
import System.Timeout (timeout)

data ReceiptAIOutcome
  = ReceiptAICompleted
  | ReceiptAIInvalidResponse
  | ReceiptAIRefused
  | ReceiptAIHttpError
  | ReceiptAITransportError
  | ReceiptAITimeout
  | ReceiptAIInterrupted
  deriving (Eq, Show)

data ReceiptAIUsage = ReceiptAIUsage
  { usageInputTokens :: !(Maybe Int64),
    usageCachedInputTokens :: !(Maybe Int64),
    usageOutputTokens :: !(Maybe Int64),
    usageReasoningTokens :: !(Maybe Int64)
  }
  deriving (Eq, Show)

data ReceiptAIAttempt = ReceiptAIAttempt
  { attemptOutcome :: !ReceiptAIOutcome,
    attemptTitle :: !(Maybe Text),
    attemptEvidence :: !(Maybe ReceiptEvidence),
    attemptUsage :: !(Maybe ReceiptAIUsage),
    attemptActualModel :: !(Maybe Text),
    attemptProviderRequestId :: !(Maybe Text),
    attemptHttpStatus :: !(Maybe Int),
    attemptDurationMs :: !Int64
  }

data ReceiptAIReadError = ResponseTooLarge | ProviderReadFailed
  deriving (Eq, Show)

receiptAIPricingVersion :: Text
receiptAIPricingVersion = "gpt-5.4-mini-2026-03-17-v1"

maxResponseBytes :: Int
maxResponseBytes = 1024 * 1024

-- The shared TLS manager must have managerRetryableException = const False.
buildReceiptAIRequest :: ReceiptAIContract -> ReceiptAIKey -> ValidatedReceiptImage -> Request
buildReceiptAIRequest contract key image =
  defaultRequest
    { method = "POST",
      secure = True,
      host = "api.openai.com",
      port = 443,
      path = "/v1/responses",
      queryString = "",
      redirectCount = 0,
      responseTimeout = responseTimeoutMicro 15000000,
      checkResponse = \_ _ -> pure (),
      requestHeaders =
        [ ("Authorization", "Bearer " <> TE.encodeUtf8 (receiptAIKeyText key)),
          ("Content-Type", "application/json"),
          ("Accept", "application/json")
        ],
      requestBody =
        RequestBodyLBS $
          encode $
            object
              [ "model" .= String "gpt-5.4-mini-2026-03-17",
                "reasoning" .= object ["effort" .= String "low"],
                "store" .= False,
                "max_output_tokens" .= (10000 :: Int),
                "tools" .= ([] :: [Value]),
                "input"
                  .= [ object ["role" .= String "developer", "content" .= contractInstructions contract],
                       object
                         [ "role" .= String "user",
                           "content"
                             .= [ object
                                    [ "type" .= String "input_image",
                                      "image_url" .= ("data:image/jpeg;base64," <> receiptImageDataBase64 (validatedImageInput image)),
                                      "detail" .= String "high"
                                    ]
                                ]
                         ]
                     ],
                "text"
                  .= object
                    [ "format"
                        .= object
                          [ "type" .= String "json_schema",
                            "name" .= String "receipt_extraction",
                            "strict" .= True,
                            "schema" .= contractSchema contract
                          ]
                    ]
              ]
    }

readReceiptAIResponse :: BodyReader -> IO (Either ReceiptAIReadError LBS.ByteString)
readReceiptAIResponse reader = do
  result <- tryJust readException $ go maxResponseBytes []
  pure $ either (const $ Left ProviderReadFailed) id result
  where
    go remaining chunks = do
      chunk <- brRead reader
      if BS.null chunk
        then pure $ Right $ LBS.fromChunks $ reverse chunks
        else
          if BS.length chunk > remaining
            then pure $ Left ResponseTooLarge
            else go (remaining - BS.length chunk) (chunk : chunks)
    readException exception = case fromException exception of
      Just (HttpExceptionRequest _ ResponseTimeout) -> Nothing
      Just (HttpExceptionRequest _ ConnectionTimeout) -> Nothing
      _ -> synchronousException exception

extractReceipt :: Manager -> ReceiptAIContract -> ReceiptAIKey -> ValidatedReceiptImage -> IO ReceiptAIAttempt
extractReceipt manager contract key image = extractReceiptWithDeadline 20000000 $
  withResponse (buildReceiptAIRequest contract key image) manager $ \response -> do
    let status = statusCode $ responseStatus response
        requestId = lookup "x-request-id" (responseHeaders response) >>= either (const Nothing) Just . TE.decodeUtf8'
        metadata attempt =
          attempt
            { attemptHttpStatus = Just status,
              attemptProviderRequestId = requestId >>= safeMetadata 200
            }
    body <- readReceiptAIResponse $ responseBody response
    evaluate $ case body of
      Left ResponseTooLarge -> metadata $ emptyAttempt $ if status >= 200 && status < 300 then ReceiptAIInvalidResponse else ReceiptAIHttpError
      Left ProviderReadFailed -> metadata $ emptyAttempt ReceiptAITransportError
      Right bytes -> decodeReceiptAIResponse status requestId bytes

-- Inject only I/O and its deadline; production destination and deadlines stay fixed.
extractReceiptWithDeadline :: Int -> IO ReceiptAIAttempt -> IO ReceiptAIAttempt
extractReceiptWithDeadline deadline action = do
  started <- getMonotonicTimeNSec
  result <- tryJust synchronousException $ timeout deadline $ action >>= evaluate
  finished <- getMonotonicTimeNSec
  let attempt = case result of
        Right (Just value) -> value
        Right Nothing -> emptyAttempt ReceiptAITimeout
        Left exception -> emptyAttempt $ case fromException exception of
          Just (HttpExceptionRequest _ ResponseTimeout) -> ReceiptAITimeout
          Just (HttpExceptionRequest _ ConnectionTimeout) -> ReceiptAITimeout
          _ -> ReceiptAITransportError
      duration = min (toInteger (maxBound :: Int64)) $ toInteger (finished - started) `div` 1000000
  pure attempt {attemptDurationMs = fromInteger duration}

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception = case fromException exception :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just exception

emptyAttempt :: ReceiptAIOutcome -> ReceiptAIAttempt
emptyAttempt outcome = ReceiptAIAttempt outcome Nothing Nothing Nothing Nothing Nothing Nothing 0

decodeReceiptAIResponse :: Int -> Maybe Text -> LBS.ByteString -> ReceiptAIAttempt
decodeReceiptAIResponse status requestId body =
  let initial =
        (emptyAttempt $ if successful then ReceiptAIInvalidResponse else ReceiptAIHttpError)
          { attemptHttpStatus = Just status,
            attemptProviderRequestId = requestId >>= safeMetadata 200
          }
   in if LBS.length (LBS.take (fromIntegral maxResponseBytes + 1) body) > fromIntegral maxResponseBytes
        then initial
        else case eitherDecode body of
          Right (Object envelope) ->
            let withUsage =
                  initial
                    { attemptUsage = KM.lookup "usage" envelope >>= parseUsage,
                      attemptActualModel = KM.lookup "model" envelope >>= textValue >>= safeMetadata 128
                    }
             in if not successful then withUsage else decodeOutput envelope withUsage
          _ -> initial
  where
    successful = status >= 200 && status < 300

textValue :: Value -> Maybe Text
textValue (String text) = Just text
textValue _ = Nothing

safeMetadata :: Int -> Text -> Maybe Text
safeMetadata limit text
  | T.null text || T.length text > limit = Nothing
  | T.all allowed text = Just text
  | otherwise = Nothing
  where
    allowed character =
      (character >= 'a' && character <= 'z')
        || (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9')
        || character `elem` ['-', '_', '.', ':']

parseUsage :: Value -> Maybe ReceiptAIUsage
parseUsage (Object fields) =
  let input = count $ KM.lookup "input_tokens" fields
      output = count $ KM.lookup "output_tokens" fields
      cached = count $ nested "input_tokens_details" "cached_tokens"
      reasoning = count $ nested "output_tokens_details" "reasoning_tokens"
      consistent child parent = case (child, parent) of
        (Just value, Just total) | value > total -> Nothing
        _ -> child
      nested key field = case KM.lookup key fields of
        Just (Object details) -> KM.lookup field details
        _ -> Nothing
   in Just $ ReceiptAIUsage input (consistent cached input) output (consistent reasoning output)
parseUsage _ = Nothing

count :: Maybe Value -> Maybe Int64
count (Just (Number number)) = do
  value <- toBoundedInteger number
  if value >= 0 then Just value else Nothing
count _ = Nothing

estimateReceiptAICost :: Maybe Text -> Maybe ReceiptAIUsage -> Maybe Int64
estimateReceiptAICost model usage = do
  unless (model == Just "gpt-5.4-mini-2026-03-17") Nothing
  counters <- usage
  input <- usageInputTokens counters
  cached <- usageCachedInputTokens counters
  output <- usageOutputTokens counters
  unless (input >= 0 && cached >= 0 && output >= 0 && cached <= input) Nothing
  let cost = (toInteger input - toInteger cached) * 750 + toInteger cached * 75 + toInteger output * 4500
  if cost <= toInteger (maxBound :: Int64) then Just $ fromInteger cost else Nothing

decodeOutput :: Object -> ReceiptAIAttempt -> ReceiptAIAttempt
decodeOutput envelope attempt = case KM.lookup "output" envelope of
  Just (Array output) ->
    let messages = [message | Object message <- V.toList output, KM.lookup "type" message == Just (String "message")]
        contents = concatMap messageContent messages
        refused = any (\content -> KM.lookup "type" content == Just (String "refusal")) contents
        texts = mapMaybe (\content -> if KM.lookup "type" content == Just (String "output_text") then KM.lookup "text" content >>= textValue else Nothing) contents
        complete =
          KM.lookup "status" envelope == Just (String "completed")
            && length messages == 1
            && all (\message -> KM.lookup "role" message == Just (String "assistant") && KM.lookup "status" message == Just (String "completed")) messages
            && all
              ( \message -> case KM.lookup "content" message of
                  Just (Array content) -> V.length content == 1
                  _ -> False
              )
              messages
     in if refused
          then attempt {attemptOutcome = ReceiptAIRefused}
          else case texts of
            [text] | complete && length contents == 1 -> case decodeExtractionOutput text of
              Just output -> attempt {attemptOutcome = ReceiptAICompleted, attemptTitle = extractedTitle output, attemptEvidence = Just $ extractedEvidence output}
              Nothing -> attempt
            _ -> attempt
  _ -> attempt
  where
    messageContent message = case KM.lookup "content" message of
      Just (Array content) -> [fields | Object fields <- V.toList content]
      _ -> []

decodeExtractionOutput :: Text -> Maybe ReceiptExtractionOutput
decodeExtractionOutput text = do
  let bytes = TE.encodeUtf8 text
  guard $ BS.length bytes <= maxReceiptEvidenceBytes + 4096
  value <- either (const Nothing) Just $ A.parseOnly (extractionObject <* whitespace <* A.endOfInput) bytes
  guard $ withinDepth 0 value
  parseMaybe parseJSON value

-- Capture the original evidence bytes independently of the wrapper's byte limit.
extractionObject :: A.Parser Value
extractionObject = do
  _ <- whitespace *> A.word8 123
  first <- field
  _ <- whitespace *> A.word8 44
  second <- field
  _ <- whitespace *> A.word8 125
  unless (fst first /= fst second) $ fail "Duplicate extraction field"
  pure $ Object $ KM.fromList [first, second]
  where
    field = do
      key <- Key.fromText <$> (whitespace *> jstring)
      _ <- whitespace *> A.word8 58
      (bytes, value) <- A.match jsonNoDup
      unless (key /= "evidence" || BS.length bytes <= maxReceiptEvidenceBytes) $ fail "Receipt evidence exceeds 256 KiB"
      pure (key, value)

withinDepth :: Int -> Value -> Bool
withinDepth depth value
  | depth > 64 = False
  | otherwise = case value of
      Object fields -> all (withinDepth $ depth + 1) fields
      Array values -> all (withinDepth $ depth + 1) values
      _ -> True

whitespace :: A.Parser ()
whitespace = A.skipWhile $ \byte -> byte `elem` [32, 9, 10, 13]
