{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module ReceiptSourceTest (receiptSourceTests) where

import APITest
import Codec.Picture (PixelYCbCr8 (..), encodeJpegAtQuality, generateImage)
import Control.Exception (bracket_, finally, onException)
import Control.Monad (forM_, void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_)
import Jizhang.API.Types hiding (title, value, user, note, source)
import qualified Jizhang.API.Types.Record as R (ExpenseRecordRequest (..))
import Jizhang.ReceiptAI.Image
import Log (mkLogger, showLogMessage, shutdownLogger, waitForLogger)
import Control.Exception (bracket)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

jpeg :: BS.ByteString
jpeg = LBS.toStrict $ encodeJpegAtQuality 90 (generateImage (\_ _ -> PixelYCbCr8 220 128 128) 8 8)

manualSource :: ReceiptSourceInput
manualSource = ReceiptSourceInput (ReceiptImageInput "image/jpeg" $ decodeUtf8 $ B64.encode jpeg) Nothing Nothing Nothing

expense :: UserId -> Text -> Integer -> ExpenseRecordRequest
expense payer title amount = ExpenseRecordRequest title Nothing (MinorAmount amount) (CurrencyCode "USD") payer (read "2026-10-14") EqualSplit [RecordSplitRequest payer (MinorAmount 1)] Nothing

sourcePath :: GroupId -> ReceiptId -> String
sourcePath (GroupId gid) (ReceiptId rid) = "/groups/" <> show gid <> "/receipts/" <> show rid <> "/source"

receiptMeta :: Receipt -> Maybe ReceiptSourceMeta
receiptMeta (Receipt _ _ _ _ _ _ _ meta) = meta

receiptChildren :: Receipt -> [Record]
receiptChildren (Receipt _ _ _ _ _ children _ _) = children

assertSource :: ReceiptSourceInput -> HTTP.Response LBS.ByteString -> Assertion
assertSource expected response = do
  HTTP.responseStatus response @?= status200
  lookup "Cache-Control" (HTTP.responseHeaders response) @?= Just "no-store"
  assertBool "source round-trips exact image and snapshot" (eitherDecode (HTTP.responseBody response) == Right expected)

counts :: Connection -> IO [(Int, Int, Int, Int)]
counts conn = query_ conn "SELECT (SELECT count(*) FROM receipts), (SELECT count(*) FROM records), (SELECT count(*) FROM record_splits), (SELECT count(*) FROM receipt_sources)"

modelEvidence :: ReceiptEvidence
modelEvidence = ReceiptEvidence RetailReceipt (visible "Synthetic merchant") (visible "2026-10-14")
  (ReceiptEvidenceCurrency (Just "USD") Nothing CurrencyExplicit (Just "USD printed")) [] []
  (ReceiptEvidenceTotals absent absent (visible "29.70") absent) [] []
  where
    absent = ReceiptObservation Nothing Nothing ObservationAbsent
    visible value = ReceiptObservation (Just value) (Just value) ObservationVisible

modelReview :: ReceiptImportReview
modelReview = ReceiptImportReview ReviewTotal "Corrected merchant" "Corrected total" (CurrencyCode "USD") "2026-10-14"
  TotalPrinted (Just $ MinorAmount 2970) Nothing (MinorAmount 2970) TipBillOnly Nothing [] [] ["bill_only:tipDecision"] True

insertExtraction :: Connection -> GroupId -> ReceiptEvidence -> IO UUID
insertExtraction conn (GroupId gid) evidence = do
  extraction <- nextRandom
  validated <- validateReceiptImage maxReceiptImageBytes (receiptSourceImage manualSource)
  imageHash <- case validated of
    Right image -> pure $ validatedImageSha256 image
    Left _ -> assertFailure "Synthetic JPEG did not validate" >> fail "Invalid synthetic JPEG"
  let evidenceHash = T.pack $ show (hash (canonicalReceiptEvidence evidence) :: Digest SHA256)
  void $ execute conn
    "INSERT INTO receipt_ai_usage (id, started_at, finished_at, principal_kind, group__id, requested_model, actual_model, prompt_version, prompt_sha256, image_sha256, evidence_sha256, settings_revision, status, pricing_version, input_nano_usd_per_token, cached_input_nano_usd_per_token, output_nano_usd_per_token) VALUES (?, now(), now(), 'user', ?, 'gpt-5.4-mini-2026-03-17', 'gpt-5.4-mini-2026-03-17', 'receipt-v1', repeat('a',64), ?, ?, 0, 'completed', 'gpt-5.4-mini-2026-03-17-v1', 750, 75, 4500)"
    (extraction, gid, imageHash, evidenceHash)
  pure extraction

receiptSourceTests :: TestTree
receiptSourceTests = testGroup "Receipt source persistence"
  [ testCase "manual image is atomic, metadata-only until opened, and scoped to group authority" $
      withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        alice <- setupUser env "alice"
        payer <- fixtureUserId env "alice"
        outsider <- setupUser env "outsider"
        admin <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC alice
        group <- runClient env $ cCreateGroup tc "Source scope"
        other <- runClient env $ cCreateGroup tc "Other group"
        let gid = grpId group
            rows = [expense payer "First" 1000, expense payer "Second" 2000]
        receipt <- runClient env $ cCreateReceipt tc gid (CreateReceiptRequest "Synthetic Market groceries" "Attached" rows $ Just manualSource)
        let rid = receiptId receipt
            path = sourcePath gid rid
        receiptMeta receipt @?= Just (ReceiptSourceMeta "image/jpeg" 8 8 (BS.length jpeg) (T.pack $ show (hash jpeg :: Digest SHA256)) False)
        assertBool "envelope does not include base64" (not $ encodeUtf8 (receiptImageDataBase64 $ receiptSourceImage manualSource) `BS.isInfixOf` LBS.toStrict (encode receipt))
        fetched <- identityRequest port manager (Just alice) "GET" path Null
        assertSource manualSource fetched
        adminFetched <- identityRequest port manager (Just admin) "GET" ("/admin" <> path) Null
        assertSource manualSource adminFetched
        forM_ [(Nothing, path, status401), (Just outsider, path, status403), (Just alice, sourcePath (grpId other) rid, status404), (Just alice, "/admin" <> path, status401)] $ \(token, location, expected) -> do
          denied <- identityRequest port manager token "GET" location Null
          HTTP.responseStatus denied @?= expected
        updated <- runClient env $ cUpdateReceipt tc gid rid (UpdateReceiptRequest "Renamed receipt" "Keep" rows KeepReceiptSource)
        assertBool "keep retains metadata" (receiptMeta updated == receiptMeta receipt)
        identityRequest port manager (Just alice) "GET" path Null >>= assertSource manualSource
        _ <- runClient env $ cUpdateReceipt tc gid rid (UpdateReceiptRequest "Renamed receipt" "Remove" rows RemoveReceiptSource)
        removed <- identityRequest port manager (Just alice) "GET" path Null
        HTTP.responseStatus removed @?= status404
        _ <- runClient env $ cUpdateReceipt tc gid rid (UpdateReceiptRequest "Renamed receipt" "Replace" rows $ ReplaceReceiptSource manualSource)
        identityRequest port manager (Just alice) "GET" path Null >>= assertSource manualSource
        _ <- runClient env $ cDeleteReceipt tc gid rid
        absent <- identityRequest port manager (Just alice) "GET" path Null
        HTTP.responseStatus absent @?= status404,
    testCase "source insert/update failures roll back headers, new children and allocations on both APIs" $
      withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager conn -> do
        token <- setupUser env "alice"
        payer <- fixtureUserId env "alice"
        operator <- setupAdmin env "admin" "adminpass123"
        let user = mkTC token
            admin = mkAdminTC operator
            install = do
              void $ execute_ conn "CREATE FUNCTION fail_receipt_source() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic source write failure' USING ERRCODE = '23514'; END; $$"
              void $ execute_ conn "CREATE TRIGGER fail_receipt_source BEFORE INSERT OR UPDATE ON receipt_sources FOR EACH ROW EXECUTE FUNCTION fail_receipt_source()"
            cleanup = void (execute_ conn "DROP TRIGGER IF EXISTS fail_receipt_source ON receipt_sources") `finally` void (execute_ conn "DROP FUNCTION IF EXISTS fail_receipt_source()")
        forM_ [("Member", \gid note rows source -> cCreateReceipt user gid (CreateReceiptRequest "Synthetic Market groceries" note rows source), cUpdateReceipt user),
               ("Admin", \gid note rows source -> cAdminCreateReceipt admin gid (AdminCreateReceiptRequest payer "Synthetic Market groceries" note rows source), cAdminUpdateReceipt admin)] $ \(label, create, replace) -> do
          group <- runClient env $ cCreateGroup user label
          let gid = grpId group
              originalRows = [expense payer "Saved first" 1000, expense payer "Saved second" 2000]
          original <- runClient env $ create gid "Original note" originalRows (Just manualSource)
          originalImage <- identityRequest port manager (Just token) "GET" (sourcePath gid $ receiptId original) Null
          before <- counts conn
          bracket_ (install `onException` cleanup) cleanup $ do
            runClientExpectStatus env status500 $ create gid "Rejected" [expense payer "New first" 3000, expense payer "New second" 4000] (Just manualSource)
            counts conn >>= assertEqual "create rolls back all persisted rows" before
            runClientExpectStatus env status500 $ replace gid (receiptId original) (UpdateReceiptRequest "Must roll back title" "Must roll back" [expense payer "Changed first" 5000, expense payer "Changed second" 6000] $ ReplaceReceiptSource manualSource)
            counts conn >>= assertEqual "replacement rolls back all persisted rows" before
          restored <- runClient env $ cGetReceipt user gid (receiptId original)
          restored @?= original
          restoredImage <- identityRequest port manager (Just token) "GET" (sourcePath gid $ receiptId original) Null
          HTTP.responseBody restoredImage @?= HTTP.responseBody originalImage,
    testCase "historical receipt-v1 financial evidence digest and corrected review round-trip without a title wrapper" $
      withTestAppServer Nothing id $ \env port manager conn -> do
        token <- setupUser env "alice"
        payer <- fixtureUserId env "alice"
        let tc = mkTC token
        group <- runClient env $ cCreateGroup tc "Import provenance"
        other <- runClient env $ cCreateGroup tc "Other extraction group"
        extraction <- insertExtraction conn (grpId group) modelEvidence
        let imported = manualSource {receiptSourceExtractionId = Just extraction, receiptSourceEvidence = Just modelEvidence, receiptSourceReview = Just modelReview}
            row = (expense payer "Independent expense title" 2970) {R.date = read "2026-10-15"}
            request source = CreateReceiptRequest "Synthetic Market groceries" "Import" [row] (Just source)
        before <- counts conn
        let changed = modelEvidence {evidenceMerchant = ReceiptObservation (Just "Altered") (Just "Altered") ObservationVisible}
        runClientExpectStatus env status400 $ cCreateReceipt tc (grpId group) (request (imported {receiptSourceEvidence = Just changed}))
        runClientExpectStatus env status400 $ cCreateReceipt tc (grpId other) (request imported)
        runClientExpectStatus env status400 $ cCreateReceipt tc (grpId group) (CreateReceiptRequest "Synthetic Market groceries" "Wrong amount" [expense payer "Wrong" 2971] $ Just imported)
        counts conn >>= assertEqual "provenance and financial mismatch write nothing" before
        saved <- runClient env $ cCreateReceipt tc (grpId group) (request imported)
        identityRequest port manager (Just token) "GET" (sourcePath (grpId group) $ receiptId saved) Null >>= assertSource imported
        historical <- query_ conn "SELECT prompt_version,evidence_sha256 FROM receipt_ai_usage" :: IO [(Text, Maybe Text)]
        historical @?= [("receipt-v1", Just $ T.pack $ show (hash (canonicalReceiptEvidence modelEvidence) :: Digest SHA256))]
        snapshots <- query_ conn "SELECT evidence_json,review_json FROM receipt_sources" :: IO [(Text, Text)]
        case snapshots of
          [(originalEvidence, originalReview)] -> do
            assertBool "saved evidence is the original nine-key object" (eitherDecode (LBS.fromStrict $ encodeUtf8 originalEvidence) == Right modelEvidence)
            assertBool "saved review is unchanged" (eitherDecode (LBS.fromStrict $ encodeUtf8 originalReview) == Right modelReview)
          _ -> assertFailure "Expected one historical source snapshot"
        renamed <- runClient env $ cUpdateReceipt tc (grpId group) (receiptId saved)
          (UpdateReceiptRequest "A separately edited receipt title" "Import" [row] KeepReceiptSource)
        identityRequest port manager (Just token) "GET" (sourcePath (grpId group) $ receiptId renamed) Null >>= assertSource imported
        unchangedSnapshots <- query_ conn "SELECT evidence_json,review_json FROM receipt_sources" :: IO [(Text, Text)]
        unchangedSnapshots @?= snapshots
        unchangedDigest <- query_ conn "SELECT prompt_version,evidence_sha256 FROM receipt_ai_usage" :: IO [(Text, Maybe Text)]
        unchangedDigest @?= historical
        case receiptChildren saved of
          [ExpenseRecord {date = accountingDate, amountMinor = amount}] -> do
            accountingDate @?= read "2026-10-15"
            amount @?= MinorAmount 2970
          _ -> assertFailure "Expected one total-mode expense"
        let itemizedReview = modelReview
              { reviewMode = ReviewItemized,
                reviewTipDecision = TipNone,
                reviewActualTipMinor = Just (MinorAmount 0),
                reviewItems = [ReceiptReviewItem "a" "First reviewed item" (MinorAmount 1000) Nothing Nothing Nothing, ReceiptReviewItem "b" "Second reviewed item" (MinorAmount 2000) Nothing Nothing Nothing],
                reviewAdjustments = [ReceiptReviewAdjustment "discount" AdjustmentDiscount (MinorAmount 300) TreatmentSubtract ["a", "b"], ReceiptReviewAdjustment "tax" AdjustmentTax (MinorAmount 270) TreatmentAdd ["a", "b"]],
                reviewAcknowledgedWarnings = []
              }
            itemizedSource = imported {receiptSourceReview = Just itemizedReview}
            firstRow = expense payer "Editable first title" 990
            secondRow = (expense payer "Editable second title" 1980) {R.date = read "2026-10-16"}
        importedItems <- runClient env $ cCreateReceipt tc (grpId group) (CreateReceiptRequest "Synthetic Market groceries" "Two imported expenses" [firstRow, secondRow] $ Just itemizedSource)
        identityRequest port manager (Just token) "GET" (sourcePath (grpId group) $ receiptId importedItems) Null >>= assertSource itemizedSource
        case receiptChildren importedItems of
          [ExpenseRecord {amountMinor = firstAmount}, ExpenseRecord {amountMinor = secondAmount}] ->
            [firstAmount, secondAmount] @?= map MinorAmount [990, 1980]
          _ -> assertFailure "Expected two positive reviewed expenses"
        _ <- runClient env $ cDeleteReceipt tc (grpId group) (receiptId importedItems)
        _ <- runClient env $ cDeleteReceipt tc (grpId group) (receiptId saved)
        sources <- query_ conn "SELECT count(*) FROM receipt_sources" :: IO [Only Int]
        usage <- query_ conn "SELECT count(*) FROM receipt_ai_usage" :: IO [Only Int]
        sources @?= [Only 0]
        usage @?= [Only 1],
    testCase "trace diagnostics never include image or original evidence contents" $ do
      messages <- newIORef []
      let collect message = atomicModifyIORef' messages (\values -> (showLogMessage Nothing message : values, ()))
      bracket (mkLogger "receipt-source-privacy" collect) shutdownLogger $ \logger ->
        withTestAppServerLogger logger Nothing id $ \env port manager conn -> do
          token <- setupUser env "alice"
          payer <- fixtureUserId env "alice"
          let tc = mkTC token
              canary = "synthetic-original-evidence-secret"
              evidence = modelEvidence {evidenceMerchant = ReceiptObservation (Just canary) (Just canary) ObservationVisible}
          group <- runClient env $ cCreateGroup tc "Source privacy"
          extraction <- insertExtraction conn (grpId group) evidence
          let imported = manualSource {receiptSourceExtractionId = Just extraction, receiptSourceEvidence = Just evidence, receiptSourceReview = Just modelReview}
          saved <- runClient env $ cCreateReceipt tc (grpId group) (CreateReceiptRequest "Synthetic Market groceries" "Private attachment" [expense payer "Public expense" 2970] $ Just imported)
          identityRequest port manager (Just token) "GET" (sourcePath (grpId group) $ receiptId saved) Null >>= assertSource imported
          waitForLogger logger
          captured <- T.unlines <$> readIORef messages
          forM_ [canary, receiptImageDataBase64 $ receiptSourceImage manualSource, token] $ \secret ->
            assertBool "private source or credential leaked into logs" (not $ secret `T.isInfixOf` captured)
  ]
