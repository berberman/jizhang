{-# LANGUAGE OverloadedStrings #-}

module ReceiptAIProviderTest (receiptAIProviderTests) where

import Codec.Picture (PixelYCbCr8 (..), generateImage)
import Codec.Picture.Jpg (encodeJpegAtQuality)
import Control.Concurrent (newEmptyMVar, takeMVar)
import Control.Exception (AsyncException (..), bracket_, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson hiding (decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Jizhang.API.Types.ReceiptAI (ReceiptImageInput (..), ReceiptEvidence (..), ReceiptModelIssue (..), maxReceiptEvidenceBytes, maxReceiptImageBytes)
import Jizhang.ReceiptAI.Contract
import Jizhang.ReceiptAI.Image
import Jizhang.ReceiptAI.Provider
import Jizhang.ReceiptAI.Runtime (parseReceiptAIKey)
import Network.HTTP.Client (BodyReader, HttpException (..), HttpExceptionContent (..), Request (..), RequestBody (..), defaultRequest, responseTimeoutMicro)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

receiptAIProviderTests :: TestTree
receiptAIProviderTests = testGroup "Receipt AI provider"
  [ testCase "actual request fixes transport, image, roles, schema and storage" $ do
      contract <- loadReceiptAIContract
      key <- either (fail . T.unpack) pure $ parseReceiptAIKey "synthetic-provider-key"
      image <- testImage
      let request = buildReceiptAIRequest contract key image
      method request @?= "POST"
      secure request @?= True
      host request @?= "api.openai.com"
      port request @?= 443
      path request @?= "/v1/responses"
      queryString request @?= ""
      redirectCount request @?= 0
      responseTimeout request @?= responseTimeoutMicro 15000000
      lookup "Authorization" (requestHeaders request) @?= Just "Bearer synthetic-provider-key"
      lookup "Content-Type" (requestHeaders request) @?= Just "application/json"
      body <- case requestBody request of
        RequestBodyLBS bytes -> either fail pure $ eitherDecode bytes
        _ -> fail "Expected a finite JSON request body"
      valueAt ["model"] body @?= Just (String model)
      valueAt ["reasoning", "effort"] body @?= Just (String "low")
      valueAt ["store"] body @?= Just (Bool False)
      valueAt ["tools"] body @?= Just (toJSON ([] :: [Value]))
      valueAt ["max_output_tokens"] body @?= Just (toJSON (10000 :: Int))
      valueAt ["text", "format", "schema"] body @?= Just (contractSchema contract)
      valueAt ["text", "format", "strict"] body @?= Just (Bool True)
      valueAt ["text", "format", "type"] body @?= Just (String "json_schema")
      valueAt ["input"] body @?= Just (toJSON
        [ object ["role" .= String "developer", "content" .= contractInstructions contract],
          object ["role" .= String "user", "content" .=
            [object ["type" .= String "input_image", "detail" .= String "high",
              "image_url" .= ("data:image/jpeg;base64," <> receiptImageDataBase64 (validatedImageInput image))]]]
        ])
      checkResponse request request (error "Response must not be inspected by checkResponse"),
    testCase "completed evidence tolerates unrelated provider metadata and reasoning" $ do
      let result = decode 200 $ envelope "completed" (message $ outputText evidenceText)
      attemptOutcome result @?= ReceiptAICompleted
      assertBool "parsed evidence is present" $ isJust $ attemptEvidence result
      attemptTitle result @?= Just "Synthetic Market groceries"
      attemptUsage result @?= Just knownUsage
      attemptActualModel result @?= Just model
      attemptProviderRequestId result @?= Just "req_synthetic-1"
      attemptHttpStatus result @?= Just 200,
    testCase "nullable and normalized Unicode titles leave financial evidence unchanged" $ do
      let parse title = decode 200 $ envelope "completed" $ message $ outputText $ extractionText title financialEvidenceText
          original = parse (String "Synthetic Market groceries")
      forM_ [Null, String "  会议 <b>Market</b>  ", String (T.replicate 512 "😀")] $ \title -> do
        let result = parse title
        attemptOutcome result @?= ReceiptAICompleted
        assertBool "identical financial evidence" (attemptEvidence result == attemptEvidence original)
        attemptTitle result @?= case title of String text -> Just (T.strip text); _ -> Nothing,
    testCase "wrapper UTF-8 byte cap is independent of the financial evidence cap" $ do
      let padding = maxReceiptEvidenceBytes + 4096 - BS.length (TE.encodeUtf8 evidenceText)
          exact = evidenceText <> T.replicate padding " "
      assertBool "exact wrapper bound" (isJust $ decodeExtractionOutput exact)
      assertBool "one byte beyond wrapper bound" (not $ isJust $ decodeExtractionOutput $ exact <> " ")
      assertBool "UTF-8 rather than code-point bound" (not $ isJust $ decodeExtractionOutput $ evidenceText <> T.replicate (padding `div` 2 + 1) "　")
      let evidencePadding = maxReceiptEvidenceBytes - BS.length (TE.encodeUtf8 financialEvidenceText)
          exactEvidence = T.replicate evidencePadding " " <> financialEvidenceText
      assertBool "exact original evidence byte bound" (isJust $ decodeExtractionOutput $ extractionText Null exactEvidence)
      assertBool "one original evidence byte beyond bound" (not $ isJust $ decodeExtractionOutput $ extractionText Null $ " " <> exactEvidence)
      original <- either fail pure $ eitherDecodeStrict' $ TE.encodeUtf8 financialEvidenceText
      let oversized = original {evidenceIssues = replicate 128 (ReceiptModelIssue "field" $ T.replicate 2024 "x")}
          wrapped = extractionText Null $ TE.decodeUtf8 $ LBS.toStrict $ encode oversized
      assertBool "evidence exceeds its own cap" (LBS.length (encode oversized) > fromIntegral maxReceiptEvidenceBytes)
      assertBool "oversized evidence still fits wrapper cap" (BS.length (TE.encodeUtf8 wrapped) < maxReceiptEvidenceBytes + 4096)
      assertBool "independent evidence cap rejects it" (not $ isJust $ decodeExtractionOutput wrapped),
    testCase "refusal retains reported tokens without exposing refusal text" $ do
      let result = decode 200 $ envelope "completed" $ message $ object ["type" .= String "refusal", "refusal" .= String "private refusal body"]
      assertFailedWithUsage ReceiptAIRefused result,
    testCase "incomplete output retains reported tokens" $
      assertFailedWithUsage ReceiptAIInvalidResponse $ decode 200 $ envelope "incomplete" $ message $ outputText evidenceText,
    testCase "incomplete assistant message never returns either title or evidence" $ do
      let incomplete = object ["type" .= String "message", "role" .= String "assistant", "status" .= String "incomplete", "content" .= [outputText evidenceText]]
      assertFailedWithUsage ReceiptAIInvalidResponse $ decode 200 $ envelope "completed" incomplete,
    testCase "HTTP rejection retains reported tokens and model" $ do
      let result = decode 429 $ envelope "failed" $ object ["type" .= String "error", "message" .= String "private provider error"]
      assertFailedWithUsage ReceiptAIHttpError result
      attemptHttpStatus result @?= Just 429,
    testGroup "extraction wrapper rejects ambiguity and structural errors"
      [ testCase label $ assertFailedWithUsage ReceiptAIInvalidResponse $ decode 200 $ envelope "completed" $ message $ outputText text
      | (label, text) <-
          [ ("malformed JSON", "{"),
            ("unknown key", T.dropEnd 1 evidenceText <> ",\"extra\":true}"),
            ("old direct evidence", financialEvidenceText),
            ("missing title", "{\"evidence\":" <> financialEvidenceText <> "}"),
            ("missing evidence", "{\"title\":null}"),
            ("duplicate title", "{\"title\":null," <> T.drop 1 evidenceText),
            ("duplicate escaped title", "{\"ti\\u0074le\":null," <> T.drop 1 evidenceText),
            ("duplicate evidence", "{\"evidence\":" <> financialEvidenceText <> "," <> T.drop 1 evidenceText),
            ("duplicate nested evidence key", extractionText Null $ "{\"document_kind\":\"unknown\"," <> T.drop 1 financialEvidenceText),
            ("invalid nested evidence", extractionText (String "Valid title") "{}"),
            ("blank title", extractionText (String "　 ") financialEvidenceText),
            ("control title", extractionText (String "Market\nshop") financialEvidenceText),
            ("C1 title", extractionText (String "Market\x85shop") financialEvidenceText),
            ("line separator title", extractionText (String "Market\x2028shop") financialEvidenceText),
            ("overlong title", extractionText (String $ T.replicate 513 "😀") financialEvidenceText),
            ("wrong title type", extractionText (Bool True) financialEvidenceText),
            ("duplicate nested key", T.replace "\"status\":\"absent\"" "\"status\":\"absent\",\"status\":\"absent\"" evidenceText),
            ("invalid observation null contract", T.replace "\"absent\"" "\"visible\"" evidenceText),
            ("trailing JSON", evidenceText <> "{}"),
            ("trailing object comma", T.dropEnd 1 evidenceText <> ",}"),
            ("trailing array comma", T.replace "\"items\":[]" "\"items\":[{},]" evidenceText),
            ("excessive nesting", T.replicate 100 "[" <> "0" <> T.replicate 100 "]")
          ]
      ],
    testCase "numeric receipt strings are evidence, not silently repaired provider errors" $ do
      original <- either fail pure $ eitherDecodeStrict' $ TE.encodeUtf8 financialEvidenceText
      let malformed = case original of
            Object fields -> case KM.lookup "totals" fields of
              Just (Object totals) -> Object $ KM.insert "totals" (Object $ KM.insert "printed_total"
                (object ["raw" .= String "1x", "value" .= String "1x", "status" .= String "visible"]) totals) fields
              _ -> Null
            _ -> Null
      attemptOutcome (decode 200 $ envelope "completed" $ message $ outputText $ extractionText Null $ TE.decodeUtf8 $ LBS.toStrict $ encode malformed) @?= ReceiptAICompleted,
    testCase "missing output, missing text and multiple texts do not fabricate evidence" $ do
      forM_ [object [], envelope "completed" (message $ object ["type" .= String "output_text"]),
        envelope "completed" $ object ["type" .= String "message", "role" .= String "assistant", "status" .= String "completed",
          "content" .= [outputText evidenceText, outputText evidenceText]]] $ \body -> do
        let result = decode 200 body
        attemptOutcome result @?= ReceiptAIInvalidResponse
        assertBool "no evidence" $ not $ isJust $ attemptEvidence result,
    testCase "safe bounded metadata drops controls, whitespace and oversized identifiers" $ do
      forM_ ["request\nprivate", "request private", T.replicate 201 "x", "<private>", "秘密"] $ \requestId ->
        attemptProviderRequestId (decodeReceiptAIResponse 200 (Just requestId) $ encode $ envelope "completed" $ message $ outputText evidenceText) @?= Nothing
      attemptProviderRequestId (decodeReceiptAIResponse 200 (Just $ T.replicate 200 "x") "{}") @?= Just (T.replicate 200 "x")
      forM_ ["model\nprivate", "model private", T.replicate 129 "x", "<private>"] $ \actualModel -> do
        let body = case envelope "completed" (message $ outputText evidenceText) of
              Object fields -> Object $ KM.insert "model" (String actualModel) fields
              _ -> Null
        attemptActualModel (decode 200 body) @?= Nothing,
    testCase "malformed outer JSON and direct oversized bodies are safely rejected" $ do
      forM_ ["{", "null", "[]", "{}{}", LBS.replicate (1024 * 1024 + 1) 32] $ \body -> do
        let result = decodeReceiptAIResponse 200 Nothing body
        attemptOutcome result @?= ReceiptAIInvalidResponse
        attemptUsage result @?= Nothing,
    testGroup "bounded streamed reader"
      [ testCase "exact one MiB is accepted across chunks" $ do
          reader <- chunkReader [BS.replicate 400000 32, BS.replicate (1024 * 1024 - 400000) 32]
          result <- readReceiptAIResponse reader
          fmap LBS.length result @?= Right (1024 * 1024),
        testCase "first oversized chunk stops reading without consuming remaining body" $ do
          calls <- newIORef (0 :: Int)
          reader <- chunkReader [BS.replicate (1024 * 1024) 32, "x", error "Must not read after cap"]
          result <- readReceiptAIResponse (modifyIORef' calls (+ 1) >> reader)
          result @?= Left ResponseTooLarge
          readIORef calls >>= (@?= 2),
        testCase "read exceptions contain no raw error" $
          readReceiptAIResponse (throwIO $ userError "private response") >>= (@?= Left ProviderReadFailed),
        testCase "reader propagates asynchronous cancellation" $ do
          result <- try $ readReceiptAIResponse $ throwIO ThreadKilled
          case result of
            Left ThreadKilled -> pure ()
            Left other -> assertFailure $ show other
            Right _ -> assertFailure "Cancellation was swallowed"
      ],
    testGroup "provider deadline and cancellation"
      [ testCase "blocked reader times out and closes its resource" $ do
          blocked <- newEmptyMVar
          closed <- newIORef False
          result <- extractReceiptWithDeadline 10000 $ bracket_ (pure ()) (writeIORef closed True) $ do
            _ <- readReceiptAIResponse (takeMVar blocked)
            pure $ decode 200 $ envelope "completed" $ message $ outputText evidenceText
          attemptOutcome result @?= ReceiptAITimeout
          readIORef closed >>= (@?= True),
        testCase "HTTP response timeout remains timeout, not read or transport failure" $ do
          result <- extractReceiptWithDeadline 1000000 $ do
            _ <- readReceiptAIResponse $ throwIO $ HttpExceptionRequest defaultRequest ResponseTimeout
            pure $ decode 200 $ object []
          attemptOutcome result @?= ReceiptAITimeout,
        testCase "synchronous transport exceptions are sanitized" $ do
          result <- extractReceiptWithDeadline 1000000 $ throwIO $ userError "private key and response"
          attemptOutcome result @?= ReceiptAITransportError
          attemptUsage result @?= Nothing
          assertBool "no evidence" $ not $ isJust $ attemptEvidence result,
        testCase "asynchronous cancellation propagates after resource release" $ do
          closed <- newIORef False
          result <- try $ extractReceiptWithDeadline 1000000 $ bracket_ (pure ()) (writeIORef closed True) $ throwIO ThreadKilled
          case result of
            Left ThreadKilled -> pure ()
            Left other -> assertFailure $ show other
            Right _ -> assertFailure "Cancellation was swallowed"
          readIORef closed >>= (@?= True)
      ],
    usageTests,
    costTests
  ]

model :: Text
model = "gpt-5.4-mini-2026-03-17"

knownUsage :: ReceiptAIUsage
knownUsage = ReceiptAIUsage (Just 3000) (Just 1000) (Just 1000) (Just 500)

usageValue :: Value
usageValue = object ["input_tokens" .= (3000 :: Int), "input_tokens_details" .= object ["cached_tokens" .= (1000 :: Int)],
  "output_tokens" .= (1000 :: Int), "output_tokens_details" .= object ["reasoning_tokens" .= (500 :: Int)]]

envelope :: Text -> Value -> Value
envelope status output = object ["status" .= status, "model" .= model, "usage" .= usageValue,
  "metadata" .= object ["ignored" .= True],
  "output" .= [object ["type" .= String "reasoning", "encrypted_content" .= String "ignored private reasoning"], output]]

message :: Value -> Value
message content = object ["type" .= String "message", "role" .= String "assistant", "status" .= String "completed", "content" .= [content]]

outputText :: Text -> Value
outputText text = object ["type" .= String "output_text", "text" .= text, "annotations" .= ([] :: [Value])]

decode :: Int -> Value -> ReceiptAIAttempt
decode status = decodeReceiptAIResponse status (Just "req_synthetic-1") . encode

assertFailedWithUsage :: ReceiptAIOutcome -> ReceiptAIAttempt -> Assertion
assertFailedWithUsage outcome attempt = do
  attemptOutcome attempt @?= outcome
  assertBool "failure never returns evidence" $ not $ isJust $ attemptEvidence attempt
  attemptTitle attempt @?= Nothing
  attemptUsage attempt @?= Just knownUsage

valueAt :: [Key] -> Value -> Maybe Value
valueAt [] value = Just value
valueAt (key : rest) (Object fields) = KM.lookup key fields >>= valueAt rest
valueAt _ _ = Nothing

chunkReader :: [BS.ByteString] -> IO BodyReader
chunkReader chunks = do
  remaining <- newIORef chunks
  pure $ atomicModifyIORef' remaining $ \values -> case values of
    [] -> ([], BS.empty)
    chunk : rest -> (rest, chunk)

testImage :: IO ValidatedReceiptImage
testImage = do
  let jpeg = LBS.toStrict $ encodeJpegAtQuality 92 $ generateImage (\_ _ -> PixelYCbCr8 200 128 128) 2 2
  result <- validateReceiptImage maxReceiptImageBytes $ ReceiptImageInput "image/jpeg" $ TE.decodeUtf8 $ B64.encode jpeg
  either (fail . show) pure result

evidenceText :: Text
evidenceText = extractionText (String "Synthetic Market groceries") financialEvidenceText

extractionText :: Value -> Text -> Text
extractionText title evidence = "{\"title\":" <> TE.decodeUtf8 (LBS.toStrict $ encode title) <> ",\"evidence\":" <> evidence <> "}"

financialEvidenceText :: Text
financialEvidenceText = TE.decodeUtf8 $ LBS.toStrict $ encode $ object
  [ "document_kind" .= String "unknown", "merchant" .= absent, "transaction_date" .= absent,
    "currency" .= object ["code" .= Null, "symbol" .= Null, "basis" .= String "unknown", "evidence" .= Null],
    "items" .= ([] :: [Value]), "adjustments" .= ([] :: [Value]),
    "totals" .= object ["subtotal" .= absent, "tax_total" .= absent, "printed_total" .= absent, "final_paid" .= absent],
    "suggested_tips" .= ([] :: [Value]), "issues" .= ([] :: [Value])
  ]
  where
    absent = object ["value" .= Null, "raw" .= Null, "status" .= String "absent"]

usageTests :: TestTree
usageTests = testGroup "reported token validation"
  [ testCase "valid large integral numbers remain exact" $
      usageInputTokens <$> parseCounters "9007199254740993" "0" "9223372036854775807" "1"
        @?= Just (Just 9007199254740993),
    testCase "exponent notation is accepted only when mathematically integral" $ do
      parseCounters "3e3" "1e3" "1e3" "5e2" @?= Just knownUsage
      parseCounters "1e-1" "0" "1" "0" @?= Just (ReceiptAIUsage Nothing (Just 0) (Just 1) (Just 0)),
    testGroup "invalid independent counters become null"
      [ testCase (label <> " in " <> field) $ do
          let values = map (\name -> if name == field then invalid else "0") ["input", "cached", "output", "reasoning"]
          case values of
            [input, cached, output, reasoning] -> do
              let expected = ReceiptAIUsage (if field == "input" then Nothing else Just 0)
                    (if field == "cached" then Nothing else Just 0)
                    (if field == "output" then Nothing else Just 0)
                    (if field == "reasoning" then Nothing else Just 0)
              parseCounters input cached output reasoning @?= Just expected
            _ -> assertFailure "Counter fixture has wrong arity"
      | (label, invalid) <- [("negative", "-1"), ("fraction", "0.5"), ("overflow", "9223372036854775808"),
          ("huge exponent", "1e1000000"), ("small exponent", "1e-1000000"), ("string", "\"1\""),
          ("null", "null"), ("boolean", "true"), ("object", "{}"), ("array", "[]")],
        field <- ["input", "cached", "output", "reasoning"]
      ],
    testCase "only inconsistent child counts are invalidated" $ do
      parseCounters "1" "2" "3" "4" @?= Just (ReceiptAIUsage (Just 1) Nothing (Just 3) Nothing)
      parseCounters "null" "2" "null" "4" @?= Just (ReceiptAIUsage Nothing (Just 2) Nothing (Just 4)),
    testCase "missing usage is unknown rather than invented zero" $ do
      attemptUsage (decode 200 $ object []) @?= Nothing
      attemptUsage (decode 400 $ object ["usage" .= Null]) @?= Nothing
      attemptUsage (decode 400 $ object ["usage" .= object []]) @?= Just (ReceiptAIUsage Nothing Nothing Nothing Nothing)
  ]

parseCounters :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> Maybe ReceiptAIUsage
parseCounters input cached output reasoning = attemptUsage $ decodeReceiptAIResponse 400 Nothing $
  "{\"usage\":{\"input_tokens\":" <> input <> ",\"input_tokens_details\":{\"cached_tokens\":" <> cached
    <> "},\"output_tokens\":" <> output <> ",\"output_tokens_details\":{\"reasoning_tokens\":" <> reasoning <> "}}}"

costTests :: TestTree
costTests = testGroup "exact estimated API cost"
  [ testCase "synthetic known usage costs exactly 6075000 nano USD" $
      estimateReceiptAICost (Just model) (Just knownUsage) @?= Just 6075000,
    testCase "reasoning already included in output is not charged again" $
      estimateReceiptAICost (Just model) (Just knownUsage {usageReasoningTokens = Nothing}) @?= Just 6075000,
    testCase "zero known usage has a known zero estimate" $
      estimateReceiptAICost (Just model) (Just $ ReceiptAIUsage (Just 0) (Just 0) (Just 0) Nothing) @?= Just 0,
    testCase "cached tokens use the discounted rate, not the input rate" $
      estimateReceiptAICost (Just model) (Just $ ReceiptAIUsage (Just 1000) (Just 1000) (Just 0) Nothing) @?= Just 75000,
    testCase "largest in-range estimate remains exact before overflow" $ do
      let tokens = (maxBound :: Int64) `div` 4500
      estimateReceiptAICost (Just model) (Just $ ReceiptAIUsage (Just 0) (Just 0) (Just tokens) Nothing) @?= Just (tokens * 4500)
      estimateReceiptAICost (Just model) (Just $ ReceiptAIUsage (Just 0) (Just 0) (Just $ tokens + 1) Nothing) @?= Nothing,
    testCase "unknown model or needed counter stays unpriced" $ do
      forM_ [Nothing, Just "unknown-model"] $ \actual -> estimateReceiptAICost actual (Just knownUsage) @?= Nothing
      forM_ [Nothing, Just knownUsage {usageInputTokens = Nothing}, Just knownUsage {usageCachedInputTokens = Nothing},
        Just knownUsage {usageOutputTokens = Nothing}] $ \usage -> estimateReceiptAICost (Just model) usage @?= Nothing,
    testCase "BIGINT overflow never wraps" $
      estimateReceiptAICost (Just model) (Just $ ReceiptAIUsage (Just (maxBound :: Int64)) (Just 0) (Just 0) Nothing) @?= Nothing,
    testCase "invalid manually supplied usage cannot produce a cost" $
      forM_ [ReceiptAIUsage (Just 1) (Just 2) (Just 0) Nothing, ReceiptAIUsage (Just (-1)) (Just 0) (Just 0) Nothing,
        ReceiptAIUsage (Just 0) (Just 0) (Just (-1)) Nothing] $ \usage -> estimateReceiptAICost (Just model) (Just usage) @?= Nothing
  ]
