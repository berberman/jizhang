{-# LANGUAGE OverloadedStrings #-}

module ReceiptSourceDatabaseTest (receiptSourceDatabaseTests) where

import APITest (TC (..), fixtureUserId, grpId, mkTC, runClient, setupUser, withTestAppServer)
import Control.Exception (try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam.Postgres (runBeamPostgres, runBeamPostgresDebug)
import Database.PostgreSQL.Simple (Connection, Only (..), SqlError (..), execute, execute_, query_, withTransaction)
import qualified Jizhang.API.Types as API
import Jizhang.API.Types.ReceiptAI
import Jizhang.Database.Init (createTables)
import qualified Jizhang.Database.Receipt as Receipt
import qualified Jizhang.Database.ReceiptAI as AI
import qualified Jizhang.Database.Schema as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

receiptSourceDatabaseTests :: TestTree
receiptSourceDatabaseTests = testGroup "Receipt source database"
  [ testCase "fresh receipt titles are required and constrained without a default" $
      withReceiptFixture $ \conn userId groupId receiptId -> do
        let rejected value = do
              result <- try (void $ execute conn "UPDATE receipts SET title=? WHERE id=?" (value, receiptId)) :: IO (Either SqlError ())
              case result of
                Left failure -> sqlState failure @?= "23514"
                Right () -> assertFailure "Invalid stored title accepted"
        mapM_ rejected (["", " leading", "trailing ", "bad\n", T.replicate 513 "😀"] :: [Text])
        missing <- try (void $ execute conn "INSERT INTO receipts (id,group__id,uploaded_by__id,note,created_at) VALUES (gen_random_uuid(),?,?, '',now())" (groupId,userId)) :: IO (Either SqlError ())
        case missing of
          Left failure -> sqlState failure @?= "23502"
          Right () -> assertFailure "Missing stored title accepted"
        runBeamPostgres conn $ Receipt.updateReceiptMetadata groupId receiptId (T.replicate 512 "😀") "Independent note"
        stored <- runBeamPostgres conn $ Receipt.getReceipt groupId receiptId
        fmap S._receiptTitle stored @?= Just (T.replicate 512 "😀")
        fmap S._receiptNote stored @?= Just "Independent note"
        wrongGroup <- nextRandom
        runBeamPostgres conn $ Receipt.updateReceiptMetadata wrongGroup receiptId "Must not leak" "Must not change"
        unchanged <- runBeamPostgres conn $ Receipt.getReceipt groupId receiptId
        unchanged @?= stored,
    testCase "additive installation preserves existing receipt, FX and allocation snapshots" $
      withReceiptFixture $ \conn userId groupId receiptId -> do
        void $ execute_ conn "DROP TABLE receipt_sources; DROP TABLE receipt_ai_usage; DROP TABLE receipt_ai_settings"
        quoteId <- nextRandom
        recordId <- nextRandom
        void $ execute conn
          "INSERT INTO exchange_quotes (id,group__id,source_currency,target_currency,rate,provider,rate_date,fetched_at) VALUES (?,?,'EUR','USD',1.1,'frankfurter','2026-10-21','2026-10-21 12:00:00Z')"
          (quoteId, groupId)
        void $ execute conn
          "INSERT INTO records (id,group__id,record_kind,title,amount_minor,currency,settlement_currency,settlement_amount_minor,split_mode,exchange_rate,rate_source,rate_date,rate_fetched_at,exchange_quote__id,by__id,date,created_at,receipt__id) VALUES (?,?,'expense','Synthetic expense',1000,'EUR','USD',1100,'exact',1.1,'reference','2026-10-21','2026-10-21 12:00:00Z',?,?,'2026-10-21','2026-10-21 12:01:00Z',?)"
          (recordId, groupId, quoteId, userId, receiptId)
        void $ execute conn "INSERT INTO record_splits VALUES (?,?,1000,1000,1100)" (recordId, userId)
        before <- financialSnapshot conn
        createTables conn
        createTables conn
        after <- financialSnapshot conn
        assertEqual "all pre-feature financial values survive both installations" before after
        tables <- query_ conn "SELECT tablename FROM pg_tables WHERE schemaname = current_schema() AND tablename IN ('receipt_sources','receipt_ai_settings','receipt_ai_usage') ORDER BY tablename" :: IO [Only Text]
        assertEqual "new tables are installed" (map Only ["receipt_ai_settings", "receipt_ai_usage", "receipt_sources"]) tables
        settings <- runBeamPostgres conn AI.getReceiptAiSettings
        assertEqual "default singleton revision" 0 (S._receiptAiSettingsRevision settings)
        assertBool "new installation is disabled" (not $ S._receiptAiSettingsEnabled settings),
    testCase "metadata projects no image or JSON contents and respects group scope" $
      withReceiptFixture $ \conn _ groupId receiptId -> do
        let bytes = "synthetic-private-image-bytes"
        runBeamPostgres conn $ Receipt.putReceiptSource receiptId (manualSource bytes) bytes 30 20 digest
        -- Invalid stored JSON proves metadata is projected, not decoded from a full source row.
        void $ execute conn "UPDATE receipt_sources SET evidence_json='not-json', review_json='not-json' WHERE receipt__id=?" (Only receiptId)
        statements <- newIORef []
        metadata <- runBeamPostgresDebug (\sql -> modifyIORef' statements (sql :)) conn $ Receipt.getReceiptSourceMeta groupId receiptId
        assertEqual "bounded metadata only" (Just $ ReceiptSourceMeta "image/jpeg" 30 20 (BS.length bytes) digest True) metadata
        sql <- unlines <$> readIORef statements
        assertBool "no image blob column is selected" (not $ "image_bytes" `isInfixOf` sql)
        assertBool "no evidence JSON column is selected" (not $ "evidence_json" `isInfixOf` sql)
        wrongGroup <- nextRandom
        hidden <- runBeamPostgres conn $ Receipt.getReceiptSourceMeta wrongGroup receiptId
        assertEqual "cross-group metadata is absent" Nothing hidden
        hiddenSource <- runBeamPostgres conn $ Receipt.getReceiptSource wrongGroup receiptId
        assertBool "cross-group source is absent" (hiddenSource == Nothing),
    testCase "group receipt title projection selects only scoped IDs and titles in one query" $
      withReceiptFixture $ \conn _ groupId receiptId -> do
        statements <- newIORef []
        titles <- runBeamPostgresDebug (\sql -> modifyIORef' statements (sql :)) conn $ Receipt.getReceiptTitlesForGroup groupId
        stored <- runBeamPostgres conn $ Receipt.getReceipt groupId receiptId
        titles @?= [(receiptId, maybe (error "Expected receipt fixture") S._receiptTitle stored)]
        sql <- readIORef statements
        length sql @?= 1
        let statement = unlines sql
        mapM_ (\column -> assertBool ("not selected: " <> column) (not $ column `isInfixOf` statement)) ["note", "uploaded_by", "created_at", "receipt_sources", "image_bytes", "evidence_json", "review_json"]
        wrongGroup <- nextRandom
        hidden <- runBeamPostgres conn $ Receipt.getReceiptTitlesForGroup wrongGroup
        hidden @?= [],
    testCase "source replacement, retention, removal and parent deletion are transactional" $
      withReceiptFixture $ \conn _ groupId receiptId -> do
        let first = "first-private-source"
            second = "second-private-source"
        runBeamPostgres conn $ Receipt.putReceiptSource receiptId (manualSource first) first 20 10 digest
        original <- runBeamPostgres conn $ Receipt.getReceiptSource groupId receiptId
        assertBool "manual source round trips exact bytes" (original == Just (manualSource first))
        withTransaction conn $ runBeamPostgres conn $ do
          void $ Receipt.lockReceipt groupId receiptId
          Receipt.updateReceiptMetadata groupId receiptId "Renamed receipt" "Note-only edit"
        retained <- runBeamPostgres conn $ Receipt.getReceiptSource groupId receiptId
        assertBool "title/note edit keeps exact source" (retained == original)
        withTransaction conn $ runBeamPostgres conn $ do
          void $ Receipt.lockReceipt groupId receiptId
          Receipt.putReceiptSource receiptId (manualSource second) second 40 30 digest
        replacement <- runBeamPostgres conn $ Receipt.getReceiptSource groupId receiptId
        assertBool "replacement changes retained bytes" (replacement == Just (manualSource second))
        runBeamPostgres conn $ Receipt.removeReceiptSource receiptId
        removed <- runBeamPostgres conn $ Receipt.getReceiptSourceMeta groupId receiptId
        assertEqual "explicit removal clears metadata" Nothing removed
        runBeamPostgres conn $ Receipt.putReceiptSource receiptId (manualSource first) first 20 10 digest
        runBeamPostgres conn $ Receipt.deleteReceipt groupId receiptId
        count <- query_ conn "SELECT count(*) FROM receipt_sources" :: IO [Only Int]
        assertEqual "receipt deletion cascades source" [Only 0] count,
    testCase "settings revisions and usage finalization use conditional writes" $
      withReceiptFixture $ \conn userId groupId _ -> do
        updated <- runBeamPostgres conn $ AI.saveReceiptAiSettings 0 True (Just "synthetic-ciphertext")
        assertBool "matching revision updates" (maybe False ((== 1) . S._receiptAiSettingsRevision) updated)
        stale <- runBeamPostgres conn $ AI.saveReceiptAiSettings 0 False Nothing
        assertBool "stale revision cannot overwrite" (stale == Nothing)
        stored <- runBeamPostgres conn AI.getReceiptAiSettings
        assertEqual "retained configuration is unchanged" (Just "synthetic-ciphertext") (S._receiptAiSettingsApiKeyEncrypted stored)
        now <- getCurrentTime
        callId <- nextRandom
        let started = usageRow callId userId groupId now
            completed = started {S._receiptAiUsageFinishedAt = Just $ addUTCTime 1 now, S._receiptAiUsageStatus = "completed", S._receiptAiUsageEvidenceSha256 = Just evidenceDigest}
        runBeamPostgres conn $ AI.insertReceiptAiUsage started
        first <- runBeamPostgres conn $ AI.finishReceiptAiUsage completed
        second <- runBeamPostgres conn $ AI.finishReceiptAiUsage completed {S._receiptAiUsageStatus = "refused"}
        assertBool "one terminal update succeeds" (first && not second)
        matches <- runBeamPostgres conn $ AI.validateReceiptExtractionSource groupId callId digest evidenceDigest
        assertBool "completed matching provenance is accepted" matches
        wrongGroup <- nextRandom
        mapM_ (\(g, image, evidence) -> do
          accepted <- runBeamPostgres conn $ AI.validateReceiptExtractionSource g callId image evidence
          assertBool "mismatched provenance is rejected" (not accepted))
          [(wrongGroup, digest, evidenceDigest), (groupId, evidenceDigest, evidenceDigest), (groupId, digest, digest)],
    testCase "stale attempts are consistently interrupted and unknown without mutation" $
      withReceiptFixture $ \conn userId groupId _ -> do
        now <- getCurrentTime
        oldId <- nextRandom
        newId <- nextRandom
        let old = (usageRow oldId userId groupId $ addUTCTime (-301) now)
              {S._receiptAiUsageInputTokens = Just 3000, S._receiptAiUsageCachedInputTokens = Just 1000,
               S._receiptAiUsageOutputTokens = Just 1000, S._receiptAiUsageEstimatedCostNanoUsd = Just 6075000}
            filters = AI.ReceiptAiUsageFilter Nothing Nothing (Just groupId) (Just "interrupted")
        runBeamPostgres conn $ AI.insertReceiptAiUsage old
        runBeamPostgres conn $ AI.insertReceiptAiUsage (usageRow newId userId groupId now)
        calls <- runBeamPostgres conn $ AI.getReceiptAiUsagePage now filters 0 20
        assertEqual "effective status is filtered before pagination" [oldId] (map S._receiptAiUsageId calls)
        assertEqual "stale usage is unknown" [Nothing] (map S._receiptAiUsageInputTokens calls)
        assertEqual "stale estimates are unknown" [Nothing] (map S._receiptAiUsageEstimatedCostNanoUsd calls)
        raw <- runBeamPostgres conn $ AI.getReceiptAiUsage oldId
        assertEqual "reporting never mutates attempts" (Just "in_flight") (S._receiptAiUsageStatus <$> raw),
    testCase "group deletion removes sources and retains anonymous usage metadata" $
      withReceiptFixture $ \conn userId groupId receiptId -> do
        now <- getCurrentTime
        callId <- nextRandom
        runBeamPostgres conn $ AI.insertReceiptAiUsage (usageRow callId userId groupId now)
        let bytes = "synthetic-source"
        runBeamPostgres conn $ Receipt.putReceiptSource receiptId (manualSource bytes) bytes 10 10 digest
        void $ execute conn "DELETE FROM groups WHERE id=?" (Only groupId)
        count <- query_ conn "SELECT count(*) FROM receipt_sources" :: IO [Only Int]
        assertEqual "group cascade clears source" [Only 0] count
        retained <- runBeamPostgres conn $ AI.getReceiptAiUsage callId
        assertBool "usage row survives group deletion" (maybe False ((== Nothing) . S.unGroupId . S._receiptAiUsageGroup) retained)
        void $ execute conn "DELETE FROM users WHERE id=?" (Only userId)
        actorDeleted <- runBeamPostgres conn $ AI.getReceiptAiUsage callId
        assertBool "usage survives actor deletion without changing principal kind"
          (maybe False (\row -> S.unUserId (S._receiptAiUsageUser row) == Nothing && S._receiptAiUsagePrincipalKind row == "user") actorDeleted)
  ]

withReceiptFixture :: (Connection -> UUID -> UUID -> UUID -> IO ()) -> IO ()
withReceiptFixture action = withTestAppServer Nothing id $ \env _ _ conn -> do
  token <- setupUser env "alice"
  API.UserId userId <- fixtureUserId env "alice"
  group <- runClient env $ cCreateGroup (mkTC token) "Synthetic receipt group"
  let API.GroupId groupId = grpId group
  receipt <- runBeamPostgres conn $ Receipt.insertReceipt groupId userId "Synthetic Market groceries" "Original receipt note"
  action conn userId groupId (S._receiptId receipt)

financialSnapshot :: Connection -> IO [Only Text]
financialSnapshot conn = query_ conn
  "SELECT row_to_json(snapshot)::text FROM (SELECT 'receipt' AS kind, row_to_json(r)::text AS value FROM receipts r UNION ALL SELECT 'record', row_to_json(r)::text FROM records r UNION ALL SELECT 'split', row_to_json(s)::text FROM record_splits s UNION ALL SELECT 'quote', row_to_json(q)::text FROM exchange_quotes q) snapshot ORDER BY kind,value"

manualSource :: BS.ByteString -> ReceiptSourceInput
manualSource bytes = ReceiptSourceInput (ReceiptImageInput "image/jpeg" $ decodeUtf8 $ B64.encode bytes) Nothing Nothing Nothing

digest, evidenceDigest :: Text
digest = T.replicate 64 "a"
evidenceDigest = T.replicate 64 "b"

usageRow :: UUID -> UUID -> UUID -> UTCTime -> S.ReceiptAiUsage
usageRow callId userId groupId started = S.ReceiptAiUsage
  { S._receiptAiUsageId = callId,
    S._receiptAiUsageStartedAt = started,
    S._receiptAiUsageFinishedAt = Nothing,
    S._receiptAiUsagePrincipalKind = "user",
    S._receiptAiUsageUser = S.UserId $ Just userId,
    S._receiptAiUsageAdmin = S.AdminId Nothing,
    S._receiptAiUsageGroup = S.GroupId $ Just groupId,
    S._receiptAiUsageRequestedModel = "gpt-5.4-mini-2026-03-17",
    S._receiptAiUsageActualModel = Nothing,
    S._receiptAiUsagePromptVersion = "receipt-v2",
    S._receiptAiUsagePromptSha256 = digest,
    S._receiptAiUsageImageSha256 = digest,
    S._receiptAiUsageEvidenceSha256 = Nothing,
    S._receiptAiUsageSettingsRevision = 0,
    S._receiptAiUsageStatus = "in_flight",
    S._receiptAiUsageProviderRequestId = Nothing,
    S._receiptAiUsageHttpStatus = Nothing,
    S._receiptAiUsageInputTokens = Nothing,
    S._receiptAiUsageCachedInputTokens = Nothing,
    S._receiptAiUsageOutputTokens = Nothing,
    S._receiptAiUsageReasoningTokens = Nothing,
    S._receiptAiUsageDurationMs = Nothing,
    S._receiptAiUsagePricingVersion = "gpt-5.4-mini-2026-03-17-v1",
    S._receiptAiUsageInputNanoUsdPerToken = 750,
    S._receiptAiUsageCachedInputNanoUsdPerToken = 75,
    S._receiptAiUsageOutputNanoUsdPerToken = 4500,
    S._receiptAiUsageEstimatedCostNanoUsd = Nothing
  }
