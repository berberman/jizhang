{-# LANGUAGE OverloadedStrings #-}

module ReceiptAITest (receiptAITests) where

import APITest
import Codec.Picture (PixelYCbCr8 (..), encodeJpegAtQuality, generateImage)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, bracket_, finally, onException, throwIO, try)
import Control.Monad (forM_, void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_)
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types hiding (owner)
import qualified Jizhang.Database.ReceiptAI as D
import qualified Jizhang.Database.Schema as S
import Jizhang.ReceiptAI.Provider
import Jizhang.ReceiptAI.Runtime
import Log (mkLogger, showLogMessage, shutdownLogger, waitForLogger)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

image :: ReceiptImageInput
image = ReceiptImageInput "image/jpeg" $ decodeUtf8 $ B64.encode $ LBS.toStrict $
  encodeJpegAtQuality 90 (generateImage (\_ _ -> PixelYCbCr8 220 128 128) 12 12)

evidence :: ReceiptEvidence
evidence = ReceiptEvidence RetailReceipt (visible "Synthetic <merchant>") (visible "2026-10-14")
  (ReceiptEvidenceCurrency (Just "USD") Nothing CurrencyExplicit (Just "USD printed"))
  [ReceiptEvidenceItem "item_a" "Synthetic item" "Synthetic item 1O.00" absent absent absent (visible "1O.00") PriceUnknown [] []]
  [] (ReceiptEvidenceTotals absent absent (visible "10.00") absent) [] []
  where
    visible text = ReceiptObservation (Just text) (Just text) ObservationVisible
    absent = ReceiptObservation Nothing Nothing ObservationAbsent

usage :: ReceiptAIUsage
usage = ReceiptAIUsage (Just 3000) (Just 1000) (Just 1000) (Just 200)

completed :: ReceiptAIAttempt
completed = decodeReceiptAIResponse 200 (Just "req_synthetic") $ encode $ object
  [ "status" .= String "completed",
    "model" .= String "gpt-5.4-mini-2026-03-17",
    "usage" .= object
      [ "input_tokens" .= usageInputTokens usage,
        "input_tokens_details" .= object ["cached_tokens" .= usageCachedInputTokens usage],
        "output_tokens" .= usageOutputTokens usage,
        "output_tokens_details" .= object ["reasoning_tokens" .= usageReasoningTokens usage]
      ],
    "output" .= [object
      [ "type" .= String "message", "role" .= String "assistant", "status" .= String "completed",
        "content" .= [object ["type" .= String "output_text", "text" .= decodeUtf8 (LBS.toStrict $ encode $
          ReceiptExtractionOutput (Just "Synthetic Market groceries") evidence)]]
      ]]
  ]

path :: GroupId -> String
path (GroupId gid) = "/groups/" <> show gid <> "/receipt-extractions"

runtime :: ReceiptAIConfig
runtime = either (error . T.unpack) id $ parseReceiptAIConfig (Just "synthetic-provider-credential") Nothing

enable :: Connection -> IO ()
enable conn = void $ execute_ conn "UPDATE receipt_ai_settings SET enabled=TRUE"

financialCounts :: Connection -> IO [(Int, Int, Int)]
financialCounts conn = query_ conn "SELECT (SELECT count(*) FROM receipts), (SELECT count(*) FROM records), (SELECT count(*) FROM receipt_sources)"

receiptAITests :: TestTree
receiptAITests = testGroup "Receipt extraction"
  [ testCase "authorization and image failures precede any provider attempt; both authorities can extract without a financial save" $ do
      calls <- newIORef (0 :: Int)
      withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123")
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> pure completed}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          outsider <- setupUser env "outsider"
          admin <- setupAdmin env "admin" "adminpass123"
          group <- runClient env $ cCreateGroup (mkTC token) "Receipt extraction"
          enable conn
          let route = path $ grpId group
              post actor location input = identityRequest port manager actor "POST" location (toJSON input)
          forM_ [(Nothing, route, status401), (Just outsider, route, status403), (Just token, "/admin" <> route, status401)] $ \(actor, location, expected) -> do
            denied <- post actor location image
            HTTP.responseStatus denied @?= expected
          readIORef calls >>= (@?= 0)
          forM_ [(ReceiptImageInput "image/png" $ receiptImageDataBase64 image, status415),
                 (ReceiptImageInput "image/jpeg" $ decodeUtf8 $ B64.encode "\137PNG\r\n\SUB\n", status415),
                 (ReceiptImageInput "image/jpeg" "/9j/2Q==", status400),
                 (ReceiptImageInput "image/jpeg" $ T.replicate (4 * ((maxReceiptImageBytes + 2) `div` 3) + 4) "A", status413)] $ \(bad, expected) -> do
            rejected <- post (Just token) route bad
            HTTP.responseStatus rejected @?= expected
          readIORef calls >>= (@?= 0)
          forM_ [(token, route), (admin, "/admin" <> route)] $ \(actor, location) -> do
            response <- post (Just actor) location image
            HTTP.responseStatus response @?= status200
            lookup "Cache-Control" (HTTP.responseHeaders response) @?= Just "no-store"
            case eitherDecode (HTTP.responseBody response) of
              Right result -> do
                assertBool "immutable typed evidence is returned" (extractionEvidence result == evidence)
                extractionTitle result @?= Just "Synthetic Market groceries"
                extractionPromptVersion result @?= "receipt-v2"
                assertBool "malformed amount is a field issue, not an invented repair" (any (\issue -> reviewIssueField issue == "items.item_a.line_total" && reviewIssueSeverity issue == ReviewError) $ extractionIssues result)
              Left _ -> assertFailure "Expected extraction response"
          readIORef calls >>= (@?= 2)
          financialCounts conn >>= (@?= [(0, 0, 0)])
          rows <- query_ conn "SELECT status,input_tokens,cached_input_tokens,output_tokens,reasoning_tokens,estimated_cost_nano_usd,evidence_sha256 IS NOT NULL FROM receipt_ai_usage ORDER BY started_at" :: IO [(Text, Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer, Bool)]
          rows @?= replicate 2 ("completed", Just 3000, Just 1000, Just 1000, Just 200, Just 6075000, True)
          digests <- query_ conn "SELECT evidence_sha256 FROM receipt_ai_usage ORDER BY started_at" :: IO [Only (Maybe Text)]
          digests @?= replicate 2 (Only $ Just $ T.pack $ show (hash (canonicalReceiptEvidence evidence) :: Digest SHA256)),
    testCase "null title succeeds with one provider call and one financial-evidence-only finalization" $ do
      calls <- newIORef (0 :: Int)
      withTestAppServer Nothing
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> pure completed {attemptTitle = Nothing}}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Nullable title"
          enable conn
          let install = do
                void $ execute_ conn "CREATE TABLE synthetic_receipt_finalizations (finished BOOLEAN NOT NULL)"
                void $ execute_ conn "CREATE FUNCTION count_receipt_ai_finalization() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN INSERT INTO synthetic_receipt_finalizations VALUES (TRUE); RETURN NEW; END; $$"
                void $ execute_ conn "CREATE TRIGGER count_receipt_ai_finalization AFTER UPDATE ON receipt_ai_usage FOR EACH ROW EXECUTE FUNCTION count_receipt_ai_finalization()"
              cleanup = do
                void $ execute_ conn "DROP TRIGGER IF EXISTS count_receipt_ai_finalization ON receipt_ai_usage"
                void $ execute_ conn "DROP FUNCTION IF EXISTS count_receipt_ai_finalization()"
                void $ execute_ conn "DROP TABLE IF EXISTS synthetic_receipt_finalizations"
          bracket_ (install `onException` cleanup) cleanup $ do
            response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
            HTTP.responseStatus response @?= status200
            case eitherDecode (HTTP.responseBody response) of
              Right result -> do
                extractionTitle result @?= Nothing
                assertBool "financial evidence remains unchanged" (extractionEvidence result == evidence)
              Left _ -> assertFailure "Expected nullable-title extraction response"
            readIORef calls >>= (@?= 1)
            finalized <- query_ conn "SELECT count(*) FROM synthetic_receipt_finalizations" :: IO [Only Int]
            finalized @?= [Only 1]
            rows <- query_ conn "SELECT prompt_version,evidence_sha256 FROM receipt_ai_usage" :: IO [(Text, Maybe Text)]
            rows @?= [("receipt-v2", Just $ T.pack $ show (hash (canonicalReceiptEvidence evidence) :: Digest SHA256))]
            financialCounts conn >>= (@?= [(0, 0, 0)]),
    testCase "failure outcomes retain independently reported usage and exact estimates with no evidence or financial rows" $
      forM_ [(ReceiptAIRefused, status422), (ReceiptAIInvalidResponse, status422), (ReceiptAIHttpError, status503), (ReceiptAITransportError, status503), (ReceiptAITimeout, status504)] $ \(outcome, expected) -> do
        calls <- newIORef (0 :: Int)
        withTestAppServer Nothing
          (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> pure completed {attemptOutcome = outcome, attemptTitle = Nothing, attemptEvidence = Nothing}}) $ \env port manager conn -> do
            token <- setupUser env "alice"
            group <- runClient env $ cCreateGroup (mkTC token) "Failure usage"
            enable conn
            response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
            HTTP.responseStatus response @?= expected
            assertBool "no synthetic success response" (not $ "extractionId" `BS.isInfixOf` LBS.toStrict (HTTP.responseBody response))
            readIORef calls >>= (@?= 1)
            rows <- query_ conn "SELECT estimated_cost_nano_usd,input_tokens,evidence_sha256 FROM receipt_ai_usage" :: IO [(Maybe Integer, Maybe Integer, Maybe Text)]
            rows @?= [(Just 6075000, Just 3000, Nothing)]
            financialCounts conn >>= (@?= [(0, 0, 0)]),
    testCase "unknown usage stays unknown after a failed attempt" $
      withTestAppServer Nothing
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> pure completed {attemptOutcome = ReceiptAIRefused, attemptTitle = Nothing, attemptEvidence = Nothing, attemptUsage = Nothing}}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Unknown usage"
          enable conn
          response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
          HTTP.responseStatus response @?= status422
          rows <- query_ conn "SELECT input_tokens,cached_input_tokens,output_tokens,estimated_cost_nano_usd FROM receipt_ai_usage" :: IO [(Maybe Integer, Maybe Integer, Maybe Integer, Maybe Integer)]
          rows @?= [(Nothing, Nothing, Nothing, Nothing)],
    testCase "failed terminal persistence yields no fake success and no retry" $ do
      calls <- newIORef (0 :: Int)
      withTestAppServer Nothing
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> pure completed}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Terminal persistence failure"
          enable conn
          let install = do
                void $ execute_ conn "CREATE FUNCTION fail_receipt_ai_finalization() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic terminal failure' USING ERRCODE='23514'; END; $$"
                void $ execute_ conn "CREATE TRIGGER fail_receipt_ai_finalization BEFORE UPDATE ON receipt_ai_usage FOR EACH ROW EXECUTE FUNCTION fail_receipt_ai_finalization()"
              cleanup = void (execute_ conn "DROP TRIGGER IF EXISTS fail_receipt_ai_finalization ON receipt_ai_usage") `finally` void (execute_ conn "DROP FUNCTION IF EXISTS fail_receipt_ai_finalization()")
          bracket_ (install `onException` cleanup) cleanup $ do
            response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
            HTTP.responseStatus response @?= status503
          readIORef calls >>= (@?= 1)
          financialCounts conn >>= (@?= [(0, 0, 0)])
          void $ execute_ conn "UPDATE receipt_ai_usage SET started_at=now()-interval '6 minutes'"
          now <- getCurrentTime
          rows <- runBeamPostgres conn $ D.getReceiptAiUsagePage now (D.ReceiptAiUsageFilter Nothing Nothing Nothing (Just "interrupted")) 0 20
          case rows of
            [row] -> do
              S._receiptAiUsageStatus row @?= "interrupted"
              S._receiptAiUsageInputTokens row @?= Nothing
              S._receiptAiUsageEstimatedCostNanoUsd row @?= Nothing
            _ -> assertFailure "Expected one interrupted unknown attempt",
    testCase "usage insertion failure prevents outbound invocation" $ do
      calls <- newIORef (0 :: Int)
      withTestAppServer Nothing
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> pure completed}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Admission persistence failure"
          enable conn
          let install = do
                void $ execute_ conn "CREATE FUNCTION fail_receipt_ai_insert() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic insert failure' USING ERRCODE='23514'; END; $$"
                void $ execute_ conn "CREATE TRIGGER fail_receipt_ai_insert BEFORE INSERT ON receipt_ai_usage FOR EACH ROW EXECUTE FUNCTION fail_receipt_ai_insert()"
              cleanup = void (execute_ conn "DROP TRIGGER IF EXISTS fail_receipt_ai_insert ON receipt_ai_usage") `finally` void (execute_ conn "DROP FUNCTION IF EXISTS fail_receipt_ai_insert()")
          bracket_ (install `onException` cleanup) cleanup $ do
            response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
            HTTP.responseStatus response @?= status500
          readIORef calls >>= (@?= 0)
          rows <- query_ conn "SELECT count(*) FROM receipt_ai_usage" :: IO [Only Int]
          rows @?= [Only 0],
    testCase "asynchronous provider cancellation finalizes interrupted metadata without swallowing cancellation" $ do
      calls <- newIORef (0 :: Int)
      withTestAppServer Nothing
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> throwIO ThreadKilled}) $ \env port _ conn -> do
          token <- setupUser env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Canceled extraction"
          enable conn
          response <- bracket
            (HTTP.newManager HTTP.defaultManagerSettings {HTTP.managerRetryableException = const False})
            HTTP.closeManager
            (\manager -> try (identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)))
            :: IO (Either SomeException (HTTP.Response LBS.ByteString))
          case response of
            Left _ -> pure ()
            Right result -> assertBool "canceled extraction is not success" (statusCode (HTTP.responseStatus result) >= 500)
          readIORef calls >>= (@?= 1)
          rows <- query_ conn "SELECT status,input_tokens,estimated_cost_nano_usd FROM receipt_ai_usage" :: IO [(Text, Maybe Integer, Maybe Integer)]
          rows @?= [("interrupted", Nothing, Nothing)],
    testCase "group authorization is rechecked after a recorded successful extraction" $ do
      revoke <- newIORef (pure ())
      withTestAppServer Nothing
        (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> do action <- readIORef revoke; action; pure completed}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          owner <- fixtureUserId env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Access revoked during extraction"
          enable conn
          let GroupId gid = grpId group
              UserId uid = owner
          writeIORef revoke $ void $ execute conn "UPDATE group_members SET active=FALSE WHERE group__id=? AND user__id=?" (gid, uid)
          response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
          HTTP.responseStatus response @?= status403
          rows <- query_ conn "SELECT status,estimated_cost_nano_usd FROM receipt_ai_usage" :: IO [(Text, Maybe Integer)]
          rows @?= [("completed", Just 6075000)],
    testCase "provider exception text, image content and credential never enter Trace diagnostics" $ do
      messages <- newIORef []
      let collect message = atomicModifyIORef' messages (\values -> (showLogMessage Nothing message : values, ()))
          providerError = "synthetic-provider-error-private-canary"
      bracket (mkLogger "receipt-ai-privacy" collect) shutdownLogger $ \logger ->
        withTestAppServerLogger logger Nothing
          (\env -> env {appReceiptAIConfig = runtime, appExtractReceipt = \_ _ -> throwIO $ userError providerError}) $ \env port manager conn -> do
            token <- setupUser env "alice"
            group <- runClient env $ cCreateGroup (mkTC token) "Provider privacy"
            enable conn
            response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
            HTTP.responseStatus response @?= status503
            waitForLogger logger
            captured <- T.unlines <$> readIORef messages
            forM_ [T.pack providerError, "synthetic-provider-credential", receiptImageDataBase64 image, token] $ \secret ->
              assertBool "secret leaked into extraction diagnostics" (not $ secret `T.isInfixOf` captured),
    testCase "disabled and missing-key capability never invokes provider and still advertises manual attachment limits" $ do
      calls <- newIORef (0 :: Int)
      withTestAppServer Nothing
        (\env -> env {appExtractReceipt = \_ _ -> modifyIORef' calls (+1) >> pure completed}) $ \env port manager conn -> do
          token <- setupUser env "alice"
          group <- runClient env $ cCreateGroup (mkTC token) "Unavailable extraction"
          forM_ [False, True] $ \enabled -> do
            if enabled then enable conn else pure ()
            response <- identityRequest port manager (Just token) "POST" (path $ grpId group) (toJSON image)
            HTTP.responseStatus response @?= status503
            options <- identityRequest port manager (Just token) "GET" (path (grpId group) <> "/options") Null
            case eitherDecode (HTTP.responseBody options) of
              Right result -> do
                assertBool "unconfigured feature is unavailable" (not $ extractionAvailable result)
                extractionMaxImageBytes result @?= maxReceiptImageBytes
                extractionAcceptedMediaTypes result @?= ["image/jpeg"]
              Left _ -> assertFailure "Expected extraction options"
          readIORef calls >>= (@?= 0)
  ]
