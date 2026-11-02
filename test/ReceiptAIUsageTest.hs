{-# LANGUAGE OverloadedStrings #-}

module ReceiptAIUsageTest (receiptAIUsageTests) where

import APITest
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..), eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query_)
import Jizhang.API (app)
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types (AdminBootstrap (..), GroupId (..))
import Jizhang.Database.Connection (closeConnectionManager, newConnectionManager, withConnection)
import Log (LogLevel (LogAttention), runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp (withApplication)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

field :: Text -> Value -> Value
field name (Object object) = maybe Null id $ KM.lookup (Key.fromText name) object
field _ _ = Null

entries :: Value -> [Value]
entries (Array values) = V.toList values
entries _ = []

report :: HTTP.Response LBS.ByteString -> IO Value
report response = do
  HTTP.responseStatus response @?= status200
  lookup "Cache-Control" (HTTP.responseHeaders response) @?= Just "no-store"
  either (\message -> assertFailure message >> fail message) pure $ eitherDecode $ HTTP.responseBody response

-- Each fixture is synthetic metadata only, with immutable initial price snapshots.
seed :: Connection -> Text -> Text -> Text -> Maybe UUID.UUID -> Maybe UUID.UUID -> Maybe Int64 -> Maybe Int64 -> IO ()
seed conn ident started status group user input cost = void $ execute conn
  "INSERT INTO receipt_ai_usage (id,started_at,finished_at,principal_kind,user__id,group__id,requested_model,actual_model,prompt_version,prompt_sha256,image_sha256,evidence_sha256,settings_revision,status,input_tokens,cached_input_tokens,output_tokens,reasoning_tokens,duration_ms,pricing_version,input_nano_usd_per_token,cached_input_nano_usd_per_token,output_nano_usd_per_token,estimated_cost_nano_usd) VALUES (?::uuid,?::timestamptz,CASE WHEN ?='in_flight' THEN NULL ELSE ?::timestamptz END,'user',?,?, 'gpt-5.4-mini-2026-03-17','gpt-5.4-mini-2026-03-17','receipt-v2',repeat('a',64),repeat('b',64),repeat('c',64),0,?,?,0,1000,200,7,'gpt-5.4-mini-2026-03-17-v1',750,75,4500,?)"
  (ident, started, status, started, user, group, status, input, cost)

receiptAIUsageTests :: TestTree
receiptAIUsageTests = testGroup "Receipt AI usage"
  [ testCase "full aggregates precede paging, counters exceed BIGINT, UUID ties and UTC boundaries are deterministic" $
      withStdOutLogger $ \logger -> withTestAppServerLoggerEnv logger (Just $ AdminBootstrap "admin" "adminpass123") id $ \appEnv env port manager conn -> do
        admin <- setupAdmin env "admin" "adminpass123"
        token <- setupUser env "owner"
        group <- runClient env $ cCreateGroup (mkTC token) "Usage group"
        let GroupId gid = grpId group
            get suffix = identityRequest port manager (Just admin) "GET" ("/admin/receipt-ai/usage" <> suffix) Null >>= report
            ident n = "00000000-0000-0000-0000-00000000000" <> n
        seed conn (ident "2") "2026-10-14T01:00:00+01:00" "completed" (Just gid) Nothing (Just maxBound) (Just 6075000)
        seed conn (ident "1") "2026-10-14T00:00:00Z" "http_error" (Just gid) Nothing (Just maxBound) (Just 6075000)
        seed conn (ident "3") "2026-10-15T00:00:00Z" "refused" Nothing Nothing Nothing Nothing
        seed conn (ident "4") "2026-10-13T23:59:59Z" "refused" Nothing Nothing Nothing Nothing
        void $ execute_ conn "UPDATE receipt_ai_usage SET duration_ms=8 WHERE id='00000000-0000-0000-0000-000000000002'"
        withConnection (appConn appEnv) $ \reportConnection -> void $ execute_ reportConnection "SET TIME ZONE 'Pacific/Honolulu'"
        first <- get "?from=2026-10-14&to=2026-10-14&offset=0&limit=1"
        second <- get "?from=2026-10-14&to=2026-10-14&offset=1&limit=1"
        let summary = field "summary" first
        field "summary" second @?= summary
        field "daily" first @?= field "daily" second
        field "byModel" first @?= field "byModel" second
        field "calls" summary @?= String "2"
        field "completed" summary @?= String "1"
        field "failed" summary @?= String "1"
        field "inputTokens" summary @?= String "18446744073709551614"
        field "estimatedCostNanoUsd" summary @?= String "12150000"
        field "averageDurationMs" summary @?= Number 7
        map (field "id") (entries $ field "items" $ field "calls" first) @?= [String $ ident "1"]
        map (field "id") (entries $ field "items" $ field "calls" second) @?= [String $ ident "2"]
        map (field "day") (entries $ field "daily" first) @?= [String "2026-10-14"]
        filtered <- get $ "?groupId=" <> show gid <> "&status=http_error"
        field "calls" (field "summary" filtered) @?= String "1"
        field "estimatedCostNanoUsd" (field "summary" filtered) @?= String "6075000"
        void $ execute_ conn "UPDATE receipt_ai_usage SET actual_model='unpriced-future-model' WHERE id='00000000-0000-0000-0000-000000000003'"
        allDays <- get ""
        length (entries $ field "daily" allDays) @?= 3
        length (entries $ field "byModel" allDays) @?= 2
        void $ execute_ conn "UPDATE receipt_ai_usage SET estimated_cost_nano_usd=9223372036854775807 WHERE id IN ('00000000-0000-0000-0000-000000000001','00000000-0000-0000-0000-000000000002')"
        largeCosts <- get "?from=2026-10-14&to=2026-10-14"
        field "estimatedCostNanoUsd" (field "summary" largeCosts) @?= String "18446744073709551614"
        empty <- get "?from=2099-01-01"
        field "calls" (field "summary" empty) @?= String "0"
        field "estimatedCostNanoUsd" (field "summary" empty) @?= String "0"
        field "averageDurationMs" (field "summary" empty) @?= Null,
    testCase "stale in-flight status, aggregates and rows agree without mutation; current attempts remain in flight" $
      withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager conn -> do
        admin <- setupAdmin env "admin" "adminpass123"
        seed conn "00000000-0000-0000-0000-000000000001" "2020-01-01T00:00:00Z" "in_flight" Nothing Nothing (Just 3000) (Just 6075000)
        void $ execute_ conn "INSERT INTO receipt_ai_usage SELECT gen_random_uuid(),now(),NULL,principal_kind,user__id,admin__id,group__id,requested_model,actual_model,prompt_version,prompt_sha256,image_sha256,evidence_sha256,settings_revision,status,provider_request_id,http_status,input_tokens,cached_input_tokens,output_tokens,reasoning_tokens,duration_ms,pricing_version,input_nano_usd_per_token,cached_input_nano_usd_per_token,output_nano_usd_per_token,estimated_cost_nano_usd FROM receipt_ai_usage"
        let get status = identityRequest port manager (Just admin) "GET" ("/admin/receipt-ai/usage?status=" <> status) Null >>= report
        stale <- get "interrupted"
        let summary = field "summary" stale
        field "calls" summary @?= String "1"
        field "failed" summary @?= String "1"
        field "inFlight" summary @?= String "0"
        field "unpricedCalls" summary @?= String "1"
        field "inputTokens" summary @?= String "0"
        field "estimatedCostNanoUsd" summary @?= String "0"
        field "averageDurationMs" summary @?= Null
        forM_ (entries $ field "items" $ field "calls" stale) $ \row -> do
          field "status" row @?= String "interrupted"
          forM_ ["inputTokens", "cachedInputTokens", "outputTokens", "reasoningTokens", "durationMs", "estimatedCostNanoUsd"] $ \name -> field name row @?= Null
        current <- get "in_flight"
        field "inFlight" (field "summary" current) @?= String "1"
        stored <- query_ conn "SELECT DISTINCT status FROM receipt_ai_usage" :: IO [Only Text]
        stored @?= [Only "in_flight"],
    testCase "strict filters reject malformed inputs and separate administrator authority is mandatory" $
      withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        admin <- setupAdmin env "admin" "adminpass123"
        member <- setupUser env "member"
        forM_ [Nothing, Just member] $ \actor -> do
          response <- identityRequest port manager actor "GET" "/admin/receipt-ai/usage" Null
          HTTP.responseStatus response @?= status401
        forM_ ["from=2026-02-30", "to=2026-1-01", "from=", "to=2026-10-01&from=2026-10-02", "groupId=bad", "status=bogus", "status=", "offset=-1", "offset=1.0", "limit=0", "limit=101", "limit=", "offset=999999999999999999999999999999"] $ \query -> do
          response <- identityRequest port manager (Just admin) "GET" ("/admin/receipt-ai/usage?" <> query) Null
          HTTP.responseStatus response @?= status400,
    testCase "deleted principals and groups leave durable usage across application restart with public deleted labels" $
      withStdOutLogger $ \logger -> withTestAppServerLoggerEnv logger (Just $ AdminBootstrap "admin" "adminpass123") id $ \appEnv env port manager conn -> do
        admin <- setupAdmin env "admin" "adminpass123"
        token <- setupUser env "disposable"
        group <- runClient env $ cCreateGroup (mkTC token) "Disposable group"
        let GroupId gid = grpId group
        users <- query_ conn "SELECT id FROM users WHERE username='disposable'" :: IO [Only UUID.UUID]
        uid <- case users of [Only value] -> pure value; _ -> assertFailure "Missing synthetic owner" >> fail "owner"
        seed conn "00000000-0000-0000-0000-000000000001" "2026-10-14T00:00:00Z" "refused" (Just gid) (Just uid) Nothing Nothing
        void $ execute conn "UPDATE users SET display_name='Public receipt actor',email='private-receipt-actor@example.test' WHERE id=?" (Only uid)
        publicResponse <- identityRequest port manager (Just admin) "GET" "/admin/receipt-ai/usage" Null
        public <- report publicResponse
        map (field "actorLabel") (entries $ field "items" $ field "calls" public) @?= [String "Public receipt actor"]
        map (field "groupName") (entries $ field "items" $ field "calls" public) @?= [String "Disposable group"]
        assertBool "report never exposes private email" $ not $ "private-receipt-actor@example.test" `BS.isInfixOf` LBS.toStrict (HTTP.responseBody publicResponse)
        void $ execute conn "DELETE FROM groups WHERE id=?" (Only gid)
        void $ execute conn "DELETE FROM users WHERE id=?" (Only uid)
        response <- identityRequest port manager (Just admin) "GET" "/admin/receipt-ai/usage" Null >>= report
        field "unpricedCalls" (field "summary" response) @?= String "1"
        case entries $ field "items" $ field "calls" response of
          [row] -> do
            field "actorLabel" row @?= String "Deleted user"
            field "actorKind" row @?= String "user"
            field "groupId" row @?= Null
            field "groupName" row @?= Null
            field "estimatedCostNanoUsd" row @?= Null
          _ -> assertFailure "Expected retained usage"
        connectionString <- BS.pack . fromMaybe "dbname=jizhang_test" <$> lookupEnv "TEST_DATABASE_URL"
        bracket (newConnectionManager connectionString) closeConnectionManager $ \freshConnections -> do
          restarted <- runLogT "usage-restart" logger LogAttention $ app appEnv {appConn = freshConnections}
          withApplication (pure restarted) $ \freshPort -> do
            retained <- identityRequest freshPort manager (Just admin) "GET" "/admin/receipt-ai/usage" Null >>= report
            retained @?= response
  ]
