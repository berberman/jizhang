{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Destructive PostgreSQL recovery regressions: dead backends, transaction outcomes,
-- cancellation and shutdown must preserve callback-at-most-once behavior and safe errors.
module DatabaseRecoveryTest (databaseRecoveryTests) where

import APITest (TC (..), grpId, mkTC, runClient, setupUser, withTestAppServerLoggerEnv)
import Control.Concurrent (ThreadId, forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, bracket_, displayException, fromException, try)
import Control.Monad (forM_, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Database.PostgreSQL.Simple (Connection, Only (..), SqlError (..), execute_, query, query_, withTransaction)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (BlockReason (BlockedOnMVar), ThreadStatus (ThreadBlocked), threadStatus)
import Jizhang.API.App (AppEnv (..))
import Jizhang.Database.Connection (DatabaseUnavailable (..), closeConnectionManager, newConnectionManager, withConnection)
import Log (mkLogger, showLogMessage, shutdownLogger, waitForLogger)
import Log.Backend.StandardOutput (withStdOutLogger)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import Servant.Client (ClientEnv)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Use a separate control connection to kill only backends in the disposable database.
-- TCP-handshake cases use a local listener rather than a live PostgreSQL outage.
databaseRecoveryTests :: TestTree
databaseRecoveryTests =
  testGroup
    "Database recovery"
    [ testCase "dead readiness and authenticated requests fail safely, then recover with the original token" $ bounded $ do
        messages <- newIORef []
        let collect message = atomicModifyIORef' messages (\old -> (showLogMessage Nothing message : old, ()))
        bracket (mkLogger "database-recovery-capture" collect) shutdownLogger $ \logger ->
          withTestAppServerLoggerEnv logger Nothing id $ \environment clientEnv port manager control -> do
            token <- setupUser clientEnv "alice"
            let client = mkTC token
            originalPid <- withConnection environment.appConn backendPid
            terminateBackend control originalPid
            live <- get manager port "/currencies"
            assertEqual "database-free catalog remains available" 200 (statusCode $ HTTP.responseStatus live)
            unavailable <- get manager port "/auth/options"
            assertUnavailableResponse unavailable
            ready <- get manager port "/auth/options"
            assertEqual "next readiness request reconnects" 200 (statusCode $ HTTP.responseStatus ready)
            void $ runClient clientEnv (cGetMe client)
            replacementPid <- withConnection environment.appConn backendPid
            assertBool "recovery uses a new PostgreSQL backend" (replacementPid /= originalPid)
            terminateBackend control replacementPid
            failedRead <- HTTP.parseRequest (baseUrl port <> "/users/me")
            readResponse <-
              HTTP.httpLbs
                failedRead
                  { HTTP.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)],
                    HTTP.redirectCount = 0
                  }
                manager
            assertUnavailableResponse readResponse
            void $ runClient clientEnv (cGetMe client)
            group <- runClient clientEnv (cCreateGroup client "Recovered group")
            void $ runClient clientEnv (cGetGroupById client $ grpId group)
            rows <- query_ control "SELECT count(*) FROM groups WHERE name = 'Recovered group'" :: IO [Only Int]
            assertEqual "new mutation is committed exactly once" [Only 1] rows
            waitForLogger logger
            captured <- T.unlines <$> readIORef messages
            connectionString <- lookupEnv "TEST_DATABASE_URL"
            let privateValues = [token, "password123", "alice@example.test"] <> map T.pack (maybeToList connectionString)
                outageBodies = map (T.pack . LBS8.unpack . HTTP.responseBody) [unavailable, readResponse]
            forM_ privateValues $ \secret ->
              forM_ (captured : outageBodies) $ \output ->
                assertBool "database failure disclosed a private value" (not $ secret `T.isInfixOf` output),
      testCase "terminated transaction rolls back without replay and the next checkout can write" $
        bounded $
          withRecoveryApp $ \environment _ _ _ control -> withProbe control $ do
            attempts <- newIORef (0 :: Int)
            inserted <- newEmptyMVar
            release <- newEmptyMVar
            let action = withConnection environment.appConn $ \connection -> do
                  atomicModifyIORef' attempts (\n -> (n + 1, ()))
                  withTransaction connection $ do
                    void $ execute_ connection "INSERT INTO database_recovery_probe VALUES (1)"
                    backendPid connection >>= putMVar inserted
                    takeMVar release
                    void (query_ connection "SELECT 1" :: IO [Only Int])
            withWorker action $ \_ finished -> do
              pid <- takeMVar inserted
              terminateBackend control pid
              putMVar release ()
              takeMVar finished >>= assertUnavailableOutcome
            assertEqual "interrupted callback ran once" 1 =<< readIORef attempts
            assertProbeCount control 0
            withConnection environment.appConn $ \connection -> do
              assertProbeCount connection 0
              void $ execute_ connection "INSERT INTO database_recovery_probe VALUES (2)"
            assertProbeCount control 1,
      testCase "committed write survives a lost callback confirmation without replay" $
        bounded $
          withRecoveryApp $ \environment _ _ _ control -> withProbe control $ do
            attempts <- newIORef (0 :: Int)
            committed <- newEmptyMVar
            release <- newEmptyMVar
            let action = withConnection environment.appConn $ \connection -> do
                  atomicModifyIORef' attempts (\n -> (n + 1, ()))
                  withTransaction connection $
                    void $
                      execute_ connection "INSERT INTO database_recovery_probe VALUES (1)"
                  backendPid connection >>= putMVar committed
                  takeMVar release
                  void (query_ connection "SELECT 1" :: IO [Only Int])
            withWorker action $ \_ finished -> do
              pid <- takeMVar committed
              assertProbeCount control 1
              terminateBackend control pid
              putMVar release ()
              takeMVar finished >>= assertUnavailableOutcome
            assertEqual "committed callback was not replayed" 1 =<< readIORef attempts
            assertProbeCount control 1
            withConnection environment.appConn $ \connection -> do
              assertProbeCount connection 1
              void $ execute_ connection "INSERT INTO database_recovery_probe VALUES (2)"
            assertProbeCount control 2,
      testCase "constraint errors retain their SQL identity and do not replace a healthy backend" $
        bounded $
          withRecoveryApp $ \environment _ _ _ control -> withProbe control $ do
            void $ execute_ control "INSERT INTO database_recovery_probe VALUES (1)"
            originalPid <- withConnection environment.appConn backendPid
            attempts <- newIORef (0 :: Int)
            result <- try $ withConnection environment.appConn $ \connection -> do
              atomicModifyIORef' attempts (\n -> (n + 1, ()))
              execute_ connection "INSERT INTO database_recovery_probe VALUES (1)"
            case result :: Either SqlError Int64 of
              Left failure -> assertEqual "unique constraint remains an ordinary SQL error" "23505" failure.sqlState
              Right _ -> assertFailure "Duplicate key unexpectedly succeeded"
            assertEqual "failed SQL callback ran once" 1 =<< readIORef attempts
            recoveredPid <- withConnection environment.appConn backendPid
            assertEqual "constraint failure preserves the healthy backend" originalPid recoveredPid
            assertProbeCount control 1,
      testCase "cancelling a checked-out transaction propagates cancellation and releases the slot" $
        bounded $
          withRecoveryApp $ \environment _ _ _ control -> withProbe control $ do
            entered <- newEmptyMVar
            release <- newEmptyMVar
            let action = withConnection environment.appConn $ \connection -> withTransaction connection $ do
                  void $ execute_ connection "INSERT INTO database_recovery_probe VALUES (1)"
                  putMVar entered ()
                  takeMVar release
            withWorker action $ \thread finished -> do
              takeMVar entered
              killThread thread
              outcome <- takeMVar finished
              case outcome of
                Left failure -> case fromException failure of
                  Just ThreadKilled -> pure ()
                  _ -> assertFailure "Cancellation was replaced by a database error"
                Right _ -> assertFailure "Cancelled callback returned normally"
            withConnection environment.appConn $ \connection -> do
              assertProbeCount connection 0
              void $ execute_ connection "INSERT INTO database_recovery_probe VALUES (2)"
            assertProbeCount control 1,
      testCase "complete callbacks stay serialized across replacement of a dead connection" $
        bounded $
          withRecoveryApp $ \environment _ _ _ control -> do
            originalPid <- withConnection environment.appConn backendPid
            terminateBackend control originalPid
            failed <- try $ withConnection environment.appConn backendPid
            case failed :: Either DatabaseUnavailable Int of
              Left DatabaseUnavailable -> pure ()
              Right _ -> assertFailure "Dead checkout unexpectedly succeeded"
            firstEntered <- newEmptyMVar
            firstRelease <- newEmptyMVar
            secondEntered <- newEmptyMVar
            secondRelease <- newEmptyMVar
            let first = withConnection environment.appConn $ \connection -> do
                  pid <- backendPid connection
                  putMVar firstEntered pid
                  takeMVar firstRelease
                  pure pid
                second = withConnection environment.appConn $ \connection -> do
                  pid <- backendPid connection
                  putMVar secondEntered pid
                  takeMVar secondRelease
                  pure pid
            withWorker first $ \_ firstFinished -> do
              firstPid <- takeMVar firstEntered
              assertBool "first checkout replaces the dead backend" (firstPid /= originalPid)
              withWorker second $ \thread secondFinished -> do
                waitUntil $ do
                  status <- threadStatus thread
                  pure $ case status of
                    ThreadBlocked BlockedOnMVar -> True
                    _ -> False
                premature <- tryTakeMVar secondEntered
                assertEqual "second callback cannot enter while the first owns the slot" Nothing premature
                putMVar firstRelease ()
                firstResult <- takeMVar firstFinished
                assertWorkerValue "first callback finished" firstPid firstResult
                secondPid <- takeMVar secondEntered
                assertEqual "serialized callbacks share the replacement backend" firstPid secondPid
                putMVar secondRelease ()
                secondResult <- takeMVar secondFinished
                assertWorkerValue "second callback finished" firstPid secondResult,
      testCase "closing the manager releases its backend and cannot reconnect it" $
        bounded $
          withRecoveryApp $ \environment _ _ _ control -> do
            pid <- withConnection environment.appConn backendPid
            closeConnectionManager environment.appConn
            waitUntil $ null <$> activeBackend control pid
            result <- try $ withConnection environment.appConn backendPid
            case result :: Either DatabaseUnavailable Int of
              Left DatabaseUnavailable -> pure ()
              Right _ -> assertFailure "Closed manager reconnected"
            closeConnectionManager environment.appConn
            assertEqual "closed backend stays absent" [] =<< activeBackend control pid,
      testCase "failed eager establishment exposes neither DSN values nor libpq diagnostics" $ bounded $ do
        let connectionString = "host=/jizhang-recovery-missing-socket-canary dbname=jizhang_test user=recovery-user-canary password=recovery-password-canary application_name=recovery-application-canary"
        outcome <- try $ bracket (newConnectionManager connectionString) closeConnectionManager (const $ pure ())
        case outcome :: Either DatabaseUnavailable () of
          Right () -> assertFailure "Missing synthetic Unix socket unexpectedly connected"
          Left failure -> do
            let diagnostic = T.pack (displayException failure)
            assertSanitizedDiagnostic diagnostic
            forM_ ["jizhang-recovery-missing-socket-canary", "recovery-user-canary", "recovery-password-canary", "recovery-application-canary"] $ \secret ->
              assertBool "Connection establishment error disclosed a DSN value" (not $ secret `T.isInfixOf` diagnostic),
      testCase "stalled establishment is capped and closes the accepted socket" $
        bounded $
          withStalledEstablishment $ \started _ finished peer -> do
            outcome <- timeout 7000000 (takeMVar finished)
            case outcome of
              Nothing -> assertFailure "Establishment exceeded the five-second cap and scheduling allowance"
              Just result -> assertUnavailableOutcome result
            elapsed <- subtract started <$> getMonotonicTimeNSec
            assertBool "incomplete PostgreSQL handshake remained pending until the establishment cap" (elapsed >= 4000000000)
            assertBool "establishment respects its cap with scheduling allowance" (elapsed < 7000000000)
            assertPeerClosed peer,
      testCase "cancelling stalled establishment propagates cancellation and closes its socket" $
        bounded $
          withStalledEstablishment $ \_ thread finished peer -> do
            cancelled <- timeout 2000000 $ killThread thread >> takeMVar finished
            case cancelled of
              Just (Left failure) -> case fromException failure of
                Just ThreadKilled -> pure ()
                _ -> assertFailure "Establishment cancellation was replaced by a database failure"
              Just (Right ()) -> assertFailure "Cancelled establishment returned a manager"
              Nothing -> assertFailure "Establishment did not respond to cancellation"
            assertPeerClosed peer
    ]

-- | Reuse the API fixture's guarded schema reset while exposing its managed connection.
withRecoveryApp :: (AppEnv -> ClientEnv -> Int -> HTTP.Manager -> Connection -> IO a) -> IO a
withRecoveryApp action = withStdOutLogger $ \logger ->
  withTestAppServerLoggerEnv logger Nothing id action

-- | Accept and read startup bytes but never complete PostgreSQL authentication.
-- Expose the peer and worker result to distinguish cancellation from establishment timeout.
withStalledEstablishment :: (Word64 -> ThreadId -> MVar (Either SomeException ()) -> Socket.Socket -> IO a) -> IO a
withStalledEstablishment action =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
    Socket.bind listener (Socket.SockAddrInet 0 $ Socket.tupleToHostAddress (127, 0, 0, 1))
    Socket.listen listener 1
    address <- Socket.getSocketName listener
    port <- case address of
      Socket.SockAddrInet value _ -> pure value
      _ -> assertFailure "Expected an IPv4 test listener" >> fail "Unexpected test listener address"
    let connectionString =
          B8.pack $
            "hostaddr=127.0.0.1 port="
              <> show port
              <> " dbname=jizhang_test user=recovery-user-canary password=recovery-password-canary"
              <> " sslmode=disable gssencmode=disable connect_timeout=30"
        establish = bracket (newConnectionManager connectionString) closeConnectionManager (const $ pure ())
    started <- getMonotonicTimeNSec
    withWorker establish $ \thread finished ->
      bracket (fst <$> Socket.accept listener) Socket.close $ \peer -> do
        header <- receiveBytes peer 4
        let messageLength = BS.foldl' (\size byte -> size * 256 + fromIntegral byte) 0 header
        assertBool "client sent a PostgreSQL startup message" (messageLength >= 8 && messageLength <= 4096)
        startup <- receiveBytes peer (messageLength - 4)
        assertBool
          "client's PostgreSQL handshake targets only the disposable database"
          ("database\0jizhang_test\0" `BS.isInfixOf` startup)
        action started thread finished peer

-- | Read exactly the requested startup bytes, failing rather than accepting an early EOF.
receiveBytes :: Socket.Socket -> Int -> IO BS.ByteString
receiveBytes peer count = collect count []
  where
    collect 0 chunks = pure $ BS.concat $ reverse chunks
    collect remaining chunks = do
      chunk <- SocketBS.recv peer remaining
      if BS.null chunk
        then assertFailure "Establishment closed before sending its startup message" >> fail "Incomplete startup message"
        else collect (remaining - BS.length chunk) (chunk : chunks)

assertPeerClosed :: Socket.Socket -> Assertion
assertPeerClosed peer = do
  remaining <- timeout 2000000 (SocketBS.recv peer 1)
  assertEqual "failed establishment releases the actual TCP connection" (Just BS.empty) remaining

-- | Bracket a committed probe table so rollback and ambiguous-commit assertions see real rows.
withProbe :: Connection -> IO a -> IO a
withProbe control =
  bracket_
    (void $ execute_ control "CREATE TABLE database_recovery_probe (id integer PRIMARY KEY)")
    (void $ execute_ control "DROP TABLE database_recovery_probe")

backendPid :: Connection -> IO Int
backendPid connection = do
  [Only pid] <- query_ connection "SELECT pg_backend_pid()"
  pure pid

activeBackend :: Connection -> Int -> IO [Only Int]
activeBackend control pid =
  query
    control
    "SELECT pid FROM pg_stat_activity WHERE datname = 'jizhang_test' AND pid = ?"
    (Only pid)

-- | Refuse the control PID and non-test backends before requesting server-side termination.
-- Return only after PostgreSQL no longer reports the target backend.
terminateBackend :: Connection -> Int -> IO ()
terminateBackend control pid = do
  ownPid <- backendPid control
  assertBool "control connection is separate from the managed connection" (ownPid /= pid)
  assertEqual "target is a backend in the disposable database" [Only pid] =<< activeBackend control pid
  terminated <-
    query
      control
      "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'jizhang_test' AND pid = ?"
      (Only pid) ::
      IO [Only Bool]
  assertEqual "PostgreSQL accepted backend termination" [Only True] terminated
  waitUntil $ null <$> activeBackend control pid

assertProbeCount :: Connection -> Int -> Assertion
assertProbeCount connection expected = do
  rows <- query_ connection "SELECT count(*) FROM database_recovery_probe" :: IO [Only Int]
  assertEqual "persisted probe rows" [Only expected] rows

assertUnavailableOutcome :: Either SomeException a -> Assertion
assertUnavailableOutcome outcome = case outcome of
  Left failure -> case fromException failure of
    Just DatabaseUnavailable -> assertSanitizedDiagnostic (T.pack $ displayException failure)
    Nothing -> assertFailure "Connection loss did not become DatabaseUnavailable"
  Right _ -> assertFailure "Interrupted callback unexpectedly returned normally"

assertWorkerValue :: (Eq a, Show a) => String -> a -> Either SomeException a -> Assertion
assertWorkerValue label expected outcome = case outcome of
  Left _ -> assertFailure label
  Right value -> assertEqual label expected value

assertUnavailableResponse :: HTTP.Response LBS.ByteString -> Assertion
assertUnavailableResponse response = do
  assertEqual "database outage is a service-unavailable response" 503 (statusCode $ HTTP.responseStatus response)
  assertSanitizedDiagnostic (T.pack $ LBS8.unpack $ HTTP.responseBody response)
  assertEqual "outage response must not be cached" (Just "no-store") (lookup "Cache-Control" $ HTTP.responseHeaders response)

assertSanitizedDiagnostic :: T.Text -> Assertion
assertSanitizedDiagnostic diagnostic = do
  forM_ ["sqlstate", "57p01", "libpq", "postgresql", "select ", "insert ", "password=", "dbname=", "database_recovery_probe"] $ \detail ->
    assertBool "database failure disclosed a database diagnostic" (not $ detail `T.isInfixOf` T.toCaseFold diagnostic)

-- | Capture success or exceptions in an MVar and kill any surviving worker on scope exit.
withWorker :: IO a -> (ThreadId -> MVar (Either SomeException a) -> IO b) -> IO b
withWorker action use = bracket start (killThread . fst) (uncurry use)
  where
    start = do
      finished <- newEmptyMVar
      thread <- forkFinally action (putMVar finished)
      pure (thread, finished)

-- | Bound the whole scenario, including polling helpers that have no timeout of their own.
bounded :: IO a -> IO a
bounded action = do
  result <- timeout 30000000 action
  case result of
    Just value -> pure value
    Nothing -> assertFailure "Database recovery test exceeded its bounded wait" >> fail "Database recovery timeout"

waitUntil :: IO Bool -> IO ()
waitUntil condition = do
  ready <- condition
  if ready then pure () else threadDelay 1000 >> waitUntil condition

baseUrl :: Int -> String
baseUrl port = "http://127.0.0.1:" <> show port

get :: HTTP.Manager -> Int -> String -> IO (HTTP.Response LBS.ByteString)
get manager port path = do
  request <- HTTP.parseRequest (baseUrl port <> path)
  HTTP.httpLbs request {HTTP.redirectCount = 0} manager
