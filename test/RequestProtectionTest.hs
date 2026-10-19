{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Admission regressions span atomic quotas, proxy trust, body limits and bounded
-- concurrency. HTTP/financial cases use the destructive PostgreSQL test harness.
module RequestProtectionTest (requestProtectionTests) where

import APITest
import Control.Concurrent (forkIO, myThreadId, throwTo)
import Control.Concurrent.MVar
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import Control.Monad (forM_, replicateM)
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.IP (IP, toSockAddr)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Only (..), execute_, query_)
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.GroupImport (CSVData (..))
import qualified Jizhang.API.Types as API
import qualified Jizhang.API.Types.ExchangeRate as Q
import Jizhang.Config (parseRequestProtectionConfig)
import qualified Jizhang.ExchangeRate as FX
import Jizhang.RequestProtection
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status200, status500)
import Network.HTTP.Types.Status (status401, status429)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (withApplication)
import RequestProtectionCredentialTest (requestProtectionCredentialTests)
import qualified Servant.Client
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

-- | Unit clocks are nanoseconds under test control; MVar gates coordinate races
-- without relying on scheduling delays to determine which request is admitted.
requestProtectionTests :: TestTree
requestProtectionTests =
  testGroup
    "Public service protections"
    [ testCase "atomic burst, exact refill boundary and backward clock" $ do
        clock <- newIORef 0
        runtime <- newRequestProtection smallConfig (readIORef clock)
        gate <- newEmptyMVar
        results <- replicateM 12 newEmptyMVar
        mapM_ (\result -> forkIO $ readMVar gate >> checkProtectionRate runtime key >>= putMVar result) results
        putMVar gate ()
        values <- mapM takeMVar results
        length (filter (== Right ()) values) @?= 2
        length (filter (== Left (RateLimited 30)) values) @?= 10
        protectionFailureHeaders (RateLimited 30)
          @?= [("Content-Type", "text/plain; charset=utf-8"), ("Cache-Control", "no-store"), ("Retry-After", "30")]
        writeIORef clock 29000000000
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 1))
        writeIORef clock 0
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 1))
        writeIORef clock 30000000000
        checkProtectionRate runtime key >>= (@?= Right ())
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 30))
        independent <- newRequestProtection smallConfig (readIORef clock)
        checkProtectionRate independent key >>= (@?= Right ()),
      testCase "bounded live keys retain debt and expire only when full" $ do
        clock <- newIORef 0
        runtime <- newRequestProtection smallConfig {protectionMaxKeys = 2} (readIORef clock)
        let second = IpTraffic (read "198.51.100.2")
            third = IpTraffic (read "198.51.100.3")
        mapM_ (\k -> checkProtectionRate runtime k >>= (@?= Right ())) [key, key, second, second]
        mapM_ (\_ -> checkProtectionRate runtime third >>= (@?= Left ProtectionBusy)) [1 .. 10 :: Int]
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 30))
        writeIORef clock 30000000000
        checkProtectionRate runtime key >>= (@?= Right ())
        checkProtectionRate runtime third >>= (@?= Left ProtectionBusy)
        writeIORef clock 60000000000
        checkProtectionRate runtime third >>= (@?= Right ()),
      testCase "mapped IPv4 identities cannot multiply budgets" $ do
        runtime <- newRequestProtection smallConfig (pure 0)
        checkProtectionRate runtime key >>= (@?= Right ())
        checkProtectionRate runtime (IpTraffic (read "::ffff:192.0.2.1")) >>= (@?= Right ())
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 30)),
      testCase "fractional refill uses integer ceilings and deadlines do not wrap the clock" $ do
        clock <- newIORef 0
        runtime <- newRequestProtection smallConfig {protectionIpRate = RatePolicy 7 1} (readIORef clock)
        checkProtectionRate runtime key >>= (@?= Right ())
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 9))
        writeIORef clock 8571428571
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 1))
        writeIORef clock 8571428572
        checkProtectionRate runtime key >>= (@?= Right ())
        writeIORef clock maxBound
        checkProtectionRate runtime key >>= (@?= Right ())
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 9))
        writeIORef clock 0
        checkProtectionRate runtime key >>= (@?= Left (RateLimited 9)),
      testCase "invalid programmatic limits fail before returning runtime" $ do
        result <- try $ newRequestProtection smallConfig {protectionIpRate = RatePolicy 1 2} (pure 0)
        case (result :: Either SomeException RequestProtection) of
          Left _ -> pure ()
          Right _ -> assertFailure "invalid policy returned a runtime",
      testCase "forwarding trust walks right-to-left and ignores untrusted metadata" forwardingTest,
      testCase "Warp trusted clients have independent budgets and malformed chains never dispatch" forwardingWarpTest,
      testCase "Warp buffers exact caps, rejects chunked overflow and classifies complete routes" bodyTest,
      testCase "Warp admission rejects immediately and releases after errors and cancellation" admissionTest,
      testCase "expensive capacity rejects atomically without consuming global-only slots" expensiveAdmissionTest,
      testCase "liveness exemption requires exact bodyless GET and auth options stays ordinary" livenessTest,
      testCase "user sessions share UUID principal budget and revoked sessions remain 401" principalTest,
      testCase "report quote and import share costly actor budgets without mutation" expensiveTest,
      testCase "auth and bulk caps precede real financial effects on both APIs" financialBodyTest,
      testCase "blocked real provider holds global and expensive admission without waiting queue" providerAdmissionTest,
      requestProtectionCredentialTests
    ]
  where
    key = IpTraffic (read "192.0.2.1" :: IP)
    smallConfig = defaultRequestProtectionConfig {protectionIpRate = RatePolicy 2 2}

generousConfig :: RequestProtectionConfig
generousConfig =
  defaultRequestProtectionConfig
    { protectionIpRate = RatePolicy 1000000 1000000,
      protectionAuthIpRate = RatePolicy 1000000 1000000
    }

forwardingTest :: Assertion
forwardingTest = do
  let cfg = generousConfig {protectionTrustedProxies = map read ["192.0.2.0/24", "2001:db8::/32"]}
      request peer headers = Wai.defaultRequest {Wai.remoteHost = toSockAddr (read peer, 1234), Wai.requestHeaders = headers}
      resolve peer headers = resolveClientIp cfg (request peer headers)
  resolve "192.0.2.1" [("X-Forwarded-For", "198.51.100.7, 192.0.2.2")] @?= Right (read "198.51.100.7")
  resolve "192.0.2.1" [("X-Forwarded-For", "198.51.100.7, 203.0.113.8")] @?= Right (read "203.0.113.8")
  resolve "2001:db8::1" [("X-Forwarded-For", "2001:db8::2, 192.0.2.2")] @?= Right (read "2001:db8::2")
  resolve "::ffff:192.0.2.1" [("X-Forwarded-For", "198.51.100.7")] @?= Right (read "198.51.100.7")
  forM_ [[], [("X-Forwarded-For", "bad"), ("Forwarded", "for=bad"), ("X-Real-IP", "bad")]] $ \headers ->
    resolve "198.51.100.1" headers @?= Right (read "198.51.100.1")
  resolveClientIp cfg (request "198.51.100.1" []) {Wai.remoteHost = toSockAddr (read "198.51.100.1", 9876)} @?= Right (read "198.51.100.1")
  forM_
    [ "",
      "unknown",
      "192.0.2.1:80",
      "[2001:db8::1]",
      "fe80::1%lo0",
      "192.0.2.1,",
      BS.replicate 4097 49,
      BS.intercalate "," (replicate 17 "192.0.2.1"),
      "\255"
    ]
    $ \value ->
      resolve "192.0.2.1" [("X-Forwarded-For", value)] @?= Left InvalidForwarding
  resolve "192.0.2.1" [("X-Forwarded-For", "198.51.100.1"), ("x-forwarded-for", "198.51.100.2")] @?= Left InvalidForwarding

-- | Run middleware with a frozen admission clock and disable client retries so
-- every observed request corresponds to one deliberate quota/admission attempt.
withProbe :: RequestProtectionConfig -> Wai.Application -> (Int -> HC.Manager -> IO a) -> IO a
withProbe cfg application action = do
  runtime <- newRequestProtection cfg (pure 0)
  manager <- HC.newManager HC.defaultManagerSettings {HC.managerRetryableException = const False}
  withApplication (pure $ requestProtectionMiddleware runtime application) $ \port -> action port manager

probeRequest :: Int -> HC.Manager -> BS.ByteString -> String -> HC.RequestBody -> IO (HC.Response LBS.ByteString)
probeRequest port manager method path body = do
  request <- HC.parseRequest $ "http://127.0.0.1:" <> show port <> path
  HC.httpLbs request {HC.method = method, HC.requestBody = body} manager

bodyTest :: Assertion
bodyTest = do
  cfg <-
    either (assertFailure . show) pure $
      parseRequestProtectionConfig $
        M.fromList
          [ ("REQUEST_AUTH_BODY_MAX_BYTES", "8"),
            ("REQUEST_BODY_MAX_BYTES", "16"),
            ("REQUEST_BULK_BODY_MAX_BYTES", "32"),
            ("RATE_LIMIT_AUTH_PER_IP_PER_MINUTE", "1000000")
          ]
  calls <- newIORef (0 :: Int)
  let application request respond = do
        body <- Wai.strictRequestBody request
        modifyIORef' calls (+ 1)
        respond $ Wai.responseLBS status200 [] body
  withProbe cfg {protectionAuthIpRate = RatePolicy 1000000 1000000} application $ \port manager -> do
    forM_
      [ ("POST", "/auth/login", 8),
        ("POST", "/admin/auth/session/login", 8),
        ("POST", "/users/me/reauthenticate", 8),
        ("PUT", "/users/me/password", 8),
        ("POST", "/groups/g/import", 32),
        ("POST", "/admin/groups/g/import", 32),
        ("POST", "/groups/g/receipts", 32),
        ("PUT", "/admin/groups/g/receipts/r", 32),
        ("POST", "/groups/g/receipts/bulk-delete", 16),
        ("POST", "/groups/g/import/extra", 16),
        ("PUT", "/groups/g/import", 16),
        ("POST", "/unknown", 16)
      ]
      $ \(method, path, cap) -> do
        let payload = BS.replicate cap 120
        accepted <- probeRequest port manager method path (HC.RequestBodyBS payload)
        HC.responseStatus accepted @?= status200
        HC.responseBody accepted @?= LBS.fromStrict payload
        before <- readIORef calls
        rejected <- probeRequest port manager method path (HC.RequestBodyBS $ payload <> "x")
        HC.responseStatus rejected @?= protectionFailureStatus BodyTooLarge
        readIORef calls >>= (@?= before)
        chunks <- newIORef [payload, "x"]
        let pop = atomicModifyIORef' chunks $ \remaining -> case remaining of [] -> ([], ""); x : xs -> (xs, x)
        streamed <- probeRequest port manager method path (HC.RequestBodyStreamChunked $ \consume -> consume pop)
        HC.responseStatus streamed @?= protectionFailureStatus BodyTooLarge
        readIORef calls >>= (@?= before)

admissionTest :: Assertion
admissionTest = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  completed <- newEmptyMVar
  canceled <- newEmptyMVar
  cancelHold <- newEmptyMVar
  let cfg = generousConfig {protectionMaxInFlight = 1, protectionMaxExpensiveInFlight = 1}
      application request respond = case Wai.pathInfo request of
        ["hold"] -> respond $ Wai.responseStream status200 [] $ \write _ ->
          putMVar entered () >> takeMVar release >> write "done"
        ["exception"] -> throwIO $ userError "synthetic failure"
        ["cancel"] -> do
          thread <- myThreadId
          putMVar canceled thread
          _ <- takeMVar cancelHold
          respond $ Wai.responseLBS status200 [] "unexpected"
        ["error"] -> respond $ Wai.responseLBS status500 [] "error"
        _ -> respond $ Wai.responseLBS status200 [] "done"
  withProbe cfg application $ \port manager -> do
    _ <- forkIO $ probeRequest port manager "GET" "/hold" (HC.RequestBodyBS "") >>= putMVar completed
    takeMVar entered
    busy <- probeRequest port manager "GET" "/groups/g/report" (HC.RequestBodyBS "")
    HC.responseStatus busy @?= protectionFailureStatus ProtectionBusy
    lookup "Retry-After" (HC.responseHeaders busy) @?= Just "1"
    options <- probeRequest port manager "GET" "/auth/options" (HC.RequestBodyBS "")
    HC.responseStatus options @?= protectionFailureStatus ProtectionBusy
    live <- probeRequest port manager "GET" "/currencies" (HC.RequestBodyBS "")
    HC.responseStatus live @?= status200
    putMVar release ()
    takeMVar completed >>= (\response -> HC.responseStatus response @?= status200)
    forM_ ["/error", "/exception"] $ \path -> do
      _ <- try (probeRequest port manager "GET" path (HC.RequestBodyBS "")) :: IO (Either SomeException (HC.Response LBS.ByteString))
      response <- probeRequest port manager "GET" "/next" (HC.RequestBodyBS "")
      HC.responseStatus response @?= status200
    cancellation <- newEmptyMVar
    _ <- forkIO $ (try (probeRequest port manager "GET" "/cancel" (HC.RequestBodyBS "")) :: IO (Either SomeException (HC.Response LBS.ByteString))) >>= putMVar cancellation
    thread <- takeMVar canceled
    throwTo thread ThreadKilled
    _ <- takeMVar cancellation
    response <- probeRequest port manager "GET" "/next" (HC.RequestBodyBS "")
    HC.responseStatus response @?= status200

livenessTest :: Assertion
livenessTest = do
  let cfg = generousConfig {protectionIpRate = RatePolicy 3 3, protectionAuthIpRate = RatePolicy 1 1}
      application _ respond = respond $ Wai.responseLBS status200 [] "ok"
  withProbe cfg application $ \port manager -> do
    first <- probeRequest port manager "POST" "/auth/login" (HC.RequestBodyBS "")
    HC.responseStatus first @?= status200
    denied <- probeRequest port manager "POST" "/auth/login" (HC.RequestBodyBS "")
    HC.responseStatus denied @?= protectionFailureStatus (RateLimited 60)
    options <- probeRequest port manager "GET" "/auth/options" (HC.RequestBodyBS "")
    HC.responseStatus options @?= status200
    forM_ [("GET", "/auth/options", ""), ("POST", "/currencies", ""), ("GET", "/currencies", "x"), ("GET", "/currencies/extra", "")] $ \(method, path, body) -> do
      response <- probeRequest port manager method path (HC.RequestBodyBS body)
      HC.responseStatus response @?= protectionFailureStatus (RateLimited 20)
    live <- probeRequest port manager "GET" "/currencies" (HC.RequestBodyBS "")
    HC.responseStatus live @?= status200

principalTest :: Assertion
principalTest = do
  clock <- newIORef 0
  runtime <- newRequestProtection generousConfig {protectionPrincipalRate = RatePolicy 3 3} (readIORef clock)
  withTestAppServer
    (Just $ API.AdminBootstrap "operator" "operator-password")
    (\env -> env {appRequestProtection = runtime})
    $ \env port manager conn -> do
      alice <- setupUser env "alice"
      bob <- setupUser env "bob"
      secondLogin <-
        identityRequest port manager Nothing "POST" "/auth/login" $
          object ["loginIdentifier" .= ("alice" :: T.Text), "loginPassword" .= ("password123" :: T.Text)]
      second <- decodeToken secondLogin
      _ <- runClient env $ cGetMe (mkTC alice)
      _ <- runClient env $ cGetMe (mkTC second)
      _ <- runClient env $ cCreateGroup (mkTC alice) "Admitted"
      runClientExpectStatus env status429 $ cCreateGroup (mkTC second) "Rejected"
      runClientExpectStatus env status429 $ cGetMe (mkTC alice)
      _ <- runClient env $ cGetMe (mkTC bob)
      identityRequest port manager (Just alice) "GET" "/admin/users" (object []) >>= (\r -> HC.responseStatus r @?= status401)
      runClientExpectStatus env status401 $ cGetMe (mkTC "invalid")
      query_ conn "SELECT name FROM groups" >>= (@?= [Only ("Admitted" :: T.Text)])
      admin <- setupAdmin env "operator" "operator-password"
      forM_ ["/admin/auth/session/me", "/admin/users", "/admin/groups"] $ \path -> do
        response <- identityRequest port manager (Just admin) "GET" path (object [])
        HC.responseStatus response @?= status200
      response <- identityRequest port manager (Just admin) "GET" "/admin/auth/session/me" (object [])
      HC.responseStatus response @?= status429
      _ <- runClient env $ cGetMe (mkTC bob)
      _ <- execute_ conn "UPDATE user_sessions SET revoked_at = now() WHERE user__id = (SELECT id FROM users WHERE username = 'alice')"
      runClientExpectStatus env status401 $ cGetMe (mkTC alice)
      writeIORef clock 60000000000
      runClientExpectStatus env status401 $ cGetMe (mkTC second)

decodeToken :: HC.Response LBS.ByteString -> IO T.Text
decodeToken response = do
  HC.responseStatus response @?= status200
  case eitherDecode (HC.responseBody response) of
    Right (API.LoginResponse token _) -> pure token
    Left message -> assertFailure message >> fail "invalid login response"

expensiveTest :: Assertion
expensiveTest = do
  runtime <- newRequestProtection generousConfig {protectionExpensiveRate = RatePolicy 1 1} (pure 0)
  calls <- newIORef (0 :: Int)
  let provider _ _ = modifyIORef' calls (+ 1) >> pure (Left FX.ProviderUnavailable)
  withTestAppServer
    (Just $ API.AdminBootstrap "operator" "operator-password")
    (\env -> env {appRequestProtection = runtime, appFetchReferenceRate = provider})
    $ \env _ _ conn -> do
      token <- setupUser env "alice"
      adminToken <- setupAdmin env "operator" "operator-password"
      let user = mkTC token
          admin = mkAdminTC adminToken
      first <- runClient env $ cCreateGroup user "First"
      second <- runClient env $ cCreateGroup user "Second"
      _ <- runClient env $ cGetReport user (grpId first)
      runClientExpectStatus env status429 $ cExchangeQuote user (grpId second) (Q.ExchangeQuoteRequest $ API.CurrencyCode "EUR")
      runClientExpectStatus env status429 $ cImportGroupCsv user (grpId second) (CSVData "title,amount,paidBy,split,date\nRejected,10,alice,alice,2026-10-14\n")
      _ <- runClient env $ cAdminGroupReport admin (grpId first)
      runClientExpectStatus env status429 $ cAdminExchangeQuote admin (grpId second) (Q.ExchangeQuoteRequest $ API.CurrencyCode "EUR")
      runClientExpectStatus env status429 $ cAdminImportGroupCsv admin (grpId second) (CSVData "title,amount,paidBy,split,date\nRejected,10,alice,alice,2026-10-14\n")
      readIORef calls >>= (@?= 0)
      query_ conn "SELECT (SELECT count(*) FROM records), (SELECT count(*) FROM record_splits), (SELECT count(*) FROM exchange_quotes)" >>= (@?= [(0, 0, 0) :: (Int, Int, Int)])

financialBodyTest :: Assertion
financialBodyTest = do
  let cfg = generousConfig {protectionAuthBodyBytes = 256, protectionBodyBytes = 512, protectionBulkBodyBytes = 4096}
  runtime <- newRequestProtection cfg (pure 0)
  withTestAppServer
    (Just $ API.AdminBootstrap "operator" "operator-password")
    (\env -> env {appRequestProtection = runtime})
    $ \env port manager conn -> do
      token <- setupUser env "alice"
      adminToken <- setupAdmin env "operator" "operator-password"
      owner <- fixtureUserId env "alice"
      let user = mkTC token
          admin = mkAdminTC adminToken
          login = "{\"loginIdentifier\":\"missing\",\"loginPassword\":\"wrong\"}"
          pad count body = body <> BS.replicate (count - BS.length body) 32
      atCap <- apiBody port manager Nothing "POST" "/auth/login" "application/json" (HC.RequestBodyBS $ pad 256 login)
      HC.responseStatus atCap @?= status401
      oversized <- apiBody port manager Nothing "POST" "/auth/login" "application/json" (HC.RequestBodyBS $ pad 257 login)
      HC.responseStatus oversized @?= protectionFailureStatus BodyTooLarge
      let registration = LBS.toStrict $ encode $ API.RegisterRequest "rejected" "Rejected" "rejected@example.test" "password123"
      chunks <- newIORef [pad 256 registration, " "]
      let pop = atomicModifyIORef' chunks $ \remaining -> case remaining of [] -> ([], ""); x : xs -> (xs, x)
      rejectedRegistration <- apiBody port manager Nothing "POST" "/auth/register" "application/json" (HC.RequestBodyStreamChunked $ \consume -> consume pop)
      HC.responseStatus rejectedRegistration @?= protectionFailureStatus BodyTooLarge
      query_ conn "SELECT username FROM users" >>= (@?= [Only ("alice" :: T.Text)])
      group <- runClient env $ cCreateGroup user "Bulk"
      let gid@(API.GroupId uuid) = grpId group
          csv = "#" <> LBS.replicate 600 120 <> "\ntitle,amount,paidBy,split,date\nDinner,10.01,alice,alice,2026-10-14\n"
          expense = API.ExpenseRecordRequest "Receipt expense" Nothing (API.MinorAmount 1001) (API.CurrencyCode "USD") owner (read "2026-10-14") API.EqualSplit [API.RecordSplitRequest owner (API.MinorAmount 1)] Nothing
      userRows <- runClient env $ cImportGroupCsv user gid (CSVData csv)
      adminRows <- runClient env $ cAdminImportGroupCsv admin gid (CSVData csv)
      forM_ (userRows <> adminRows) $ \record -> case record of
        API.ExpenseRecord {} -> do
          record.amountMinor @?= API.MinorAmount 1001
          record.note @?= Nothing
        _ -> assertFailure "CSV changed expense semantics"
      _ <- runClient env $ cCreateReceipt user gid (API.CreateReceiptRequest (T.replicate 600 "x") [expense])
      _ <- runClient env $ cAdminCreateReceipt admin gid (API.AdminCreateReceiptRequest owner (T.replicate 600 "x") [expense])
      before <- query_ conn "SELECT (SELECT count(*) FROM records), (SELECT count(*) FROM record_splits), (SELECT count(*) FROM receipts)" :: IO [(Int, Int, Int)]
      before @?= [(4, 4, 2)]
      forM_ [(token, "/groups/"), (adminToken, "/admin/groups/")] $ \(actor, prefix) -> do
        let over = LBS.toStrict $ "#" <> LBS.replicate 4096 120 <> "\n" <> csv
        response <- apiBody port manager (Just actor) "POST" (prefix <> show uuid <> "/import?currency=USD") "text/csv" (HC.RequestBodyBS over)
        HC.responseStatus response @?= protectionFailureStatus BodyTooLarge
        csvChunks <- newIORef [BS.take 4096 over, BS.drop 4096 over]
        let nextCsvChunk = atomicModifyIORef' csvChunks $ \remaining -> case remaining of [] -> ([], ""); x : xs -> (xs, x)
        chunkedCsv <- apiBody port manager (Just actor) "POST" (prefix <> show uuid <> "/import?currency=USD") "text/csv" (HC.RequestBodyStreamChunked $ \consume -> consume nextCsvChunk)
        HC.responseStatus chunkedCsv @?= protectionFailureStatus BodyTooLarge
        let paddedExpense = pad 513 $ LBS.toStrict $ encode expense
        expenseChunks <- newIORef [BS.take 512 paddedExpense, BS.drop 512 paddedExpense]
        let nextExpenseChunk = atomicModifyIORef' expenseChunks $ \remaining -> case remaining of [] -> ([], ""); x : xs -> (xs, x)
        rejectedExpense <- apiBody port manager (Just actor) "POST" (prefix <> show uuid <> "/records/expense") "application/json" (HC.RequestBodyStreamChunked $ \consume -> consume nextExpenseChunk)
        HC.responseStatus rejectedExpense @?= protectionFailureStatus BodyTooLarge
      persisted <- query_ conn "SELECT (SELECT count(*) FROM records), (SELECT count(*) FROM record_splits), (SELECT count(*) FROM receipts)"
      persisted @?= before

apiBody :: Int -> HC.Manager -> Maybe T.Text -> BS.ByteString -> String -> BS.ByteString -> HC.RequestBody -> IO (HC.Response LBS.ByteString)
apiBody port manager token method path contentType body = do
  request <- HC.parseRequest $ "http://127.0.0.1:" <> show port <> path
  HC.httpLbs
    request
      { HC.method = method,
        HC.requestBody = body,
        HC.requestHeaders = [("Content-Type", contentType)] <> maybe [] (\value -> [("Authorization", "Bearer " <> encodeUtf8 value)]) token
      }
    manager

providerAdmissionTest :: Assertion
providerAdmissionTest = do
  runtime <- newRequestProtection generousConfig {protectionMaxInFlight = 1, protectionMaxExpensiveInFlight = 1} (pure 0)
  entered <- newEmptyMVar
  release <- newEmptyMVar
  calls <- newIORef (0 :: Int)
  rate <- either (assertFailure . T.unpack) pure (API.parseDecimalRate "1.2")
  let provider _ _ = do
        n <- atomicModifyIORef' calls (\n -> (n + 1, n))
        if n == 0 then putMVar entered () >> takeMVar release else pure ()
        pure $ Right $ FX.ReferenceRate rate (read "2026-10-14")
  withTestAppServer Nothing (\env -> env {appRequestProtection = runtime, appFetchReferenceRate = provider}) $ \env _ _ conn -> do
    token <- setupUser env "alice"
    let user = mkTC token
    group <- runClient env $ cCreateGroup user "Admission"
    completed <- newEmptyMVar
    _ <- forkIO $ Servant.Client.runClientM (cExchangeQuote user (grpId group) (Q.ExchangeQuoteRequest $ API.CurrencyCode "EUR")) env >>= putMVar completed
    timeout 5000000 (takeMVar entered) >>= (@?= Just ())
    denied <- timeout 5000000 $ runClientExpectStatus env (protectionFailureStatus ProtectionBusy) $ cExchangeQuote user (grpId group) (Q.ExchangeQuoteRequest $ API.CurrencyCode "EUR")
    denied @?= Just ()
    readIORef calls >>= (@?= 1)
    query_ conn "SELECT count(*) FROM exchange_quotes" >>= (@?= [Only (0 :: Int)])
    putMVar release ()
    result <- timeout 5000000 (takeMVar completed)
    case result of
      Just (Right _) -> pure ()
      _ -> assertFailure "admitted provider request did not finish"
    _ <- runClient env $ cExchangeQuote user (grpId group) (Q.ExchangeQuoteRequest $ API.CurrencyCode "EUR")
    query_ conn "SELECT count(*) FROM exchange_quotes" >>= (@?= [Only (2 :: Int)])

forwardingWarpTest :: Assertion
forwardingWarpTest = do
  calls <- newIORef (0 :: Int)
  cfg <-
    either (assertFailure . show) pure $
      parseRequestProtectionConfig $
        M.fromList
          [("RATE_LIMIT_IP_PER_MINUTE", "1"), ("TRUSTED_PROXY_CIDRS", "127.0.0.1/32")]
  let application _ respond = modifyIORef' calls (+ 1) >> respond (Wai.responseLBS status200 [] "ok")
      send port manager headers = do
        request <- HC.parseRequest $ "http://127.0.0.1:" <> show port <> "/probe"
        HC.httpLbs request {HC.requestHeaders = headers} manager
  withProbe cfg application $ \port manager -> do
    forM_ ["198.51.100.1", "198.51.100.2", "2001:db8::1"] $ \client -> do
      accepted <- send port manager [("X-Forwarded-For", client)]
      HC.responseStatus accepted @?= status200
      denied <- send port manager [("X-Forwarded-For", client)]
      HC.responseStatus denied @?= status429
    readIORef calls >>= (@?= 3)
    forM_
      [ [("X-Forwarded-For", "bad")],
        [("X-Forwarded-For", "198.51.100.1"), ("X-Forwarded-For", "198.51.100.2")],
        [("X-Forwarded-For", BS.replicate 4097 49)]
      ]
      $ \headers -> do
        response <- send port manager headers
        HC.responseStatus response @?= protectionFailureStatus InvalidForwarding
    readIORef calls >>= (@?= 3)
  withProbe cfg {protectionTrustedProxies = []} application $ \port manager -> do
    first <- send port manager [("X-Forwarded-For", "198.51.100.1")]
    HC.responseStatus first @?= status200
    forM_
      [ [("X-Forwarded-For", "198.51.100.2")],
        [("Forwarded", "for=198.51.100.3")],
        [("X-Real-IP", "198.51.100.4")],
        [("X-Forwarded-For", "malformed")],
        [("User-Agent", "healthcheck"), ("X-Health-Check", "true")]
      ]
      $ \headers -> do
        response <- send port manager headers
        HC.responseStatus response @?= status429
    readIORef calls >>= (@?= 4)

expensiveAdmissionTest :: Assertion
expensiveAdmissionTest = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  completed <- newEmptyMVar
  let cfg = generousConfig {protectionMaxInFlight = 2, protectionMaxExpensiveInFlight = 1}
      application request respond = case Wai.pathInfo request of
        ["groups", "g", "report"] -> putMVar entered () >> takeMVar release >> respond (Wai.responseLBS status200 [] "done")
        _ -> respond $ Wai.responseLBS status200 [] "done"
  withProbe cfg application $ \port manager -> do
    _ <- forkIO $ probeRequest port manager "GET" "/groups/g/report" (HC.RequestBodyBS "") >>= putMVar completed
    takeMVar entered
    forM_
      [ ("GET", "/groups/g/statistics"),
        ("POST", "/groups/g/exchange-quotes"),
        ("POST", "/admin/users"),
        ("POST", "/admin/admins"),
        ("PUT", "/admin/authentication"),
        ("POST", "/admin/authentication/test"),
        ("POST", "/auth/login")
      ]
      $ \(method, path) -> do
        response <- probeRequest port manager method path (HC.RequestBodyBS "")
        HC.responseStatus response @?= protectionFailureStatus ProtectionBusy
        ordinary <- probeRequest port manager "GET" "/ordinary" (HC.RequestBodyBS "")
        HC.responseStatus ordinary @?= status200
    putMVar release ()
    takeMVar completed >>= (\response -> HC.responseStatus response @?= status200)
    next <- probeRequest port manager "GET" "/groups/g/statistics" (HC.RequestBodyBS "")
    HC.responseStatus next @?= status200
