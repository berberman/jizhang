{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | HTTP regressions for ledger operations, identity, authorization and currency policy.
-- Shared fixtures run the real application against a disposable PostgreSQL schema.
module APITest
  ( apiTests,
    withTestAppServer,
    setupUser,
    fixtureUserId,
    setupAdmin,
    mkTC,
    mkAdminTC,
    TC (..),
    AdminTC (..),
    runClient,
    runClientExpectStatus,
    grpId,
    extractRecordId,
    identityRequest,
    withTestAppServerLogger,
    withTestAppServerLoggerEnv,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, bracket_, finally, onException, try)
import Control.Monad (forM_, void)
import qualified Crypto.JWT as JWT
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64.URL as B64URL
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isSpace, toLower)
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection, Only (..), SqlError (..), close, connectPostgreSQL, execute, execute_, query, query_, withTransaction)
import GHC.Clock (getMonotonicTimeNSec)
import Jizhang.API (app)
import Jizhang.API.Admin (AdminAPI)
import Jizhang.API.AdminAuth (AdminAuthAPI)
import Jizhang.API.App
import Jizhang.API.Auth (AuthAPI)
import Jizhang.API.Group (GroupAPI)
import Jizhang.API.GroupImport (CSVData (..))
import Jizhang.API.Receipt (ReceiptAPI)
import Jizhang.API.Record (RecordAPI)
import Jizhang.API.Report (ReportAPI)
import Jizhang.API.Types
import Jizhang.API.Types.ExchangeRate (ExchangeQuote, ExchangeQuoteRequest)
import qualified Jizhang.API.Types.OIDC as Oidc
import qualified Jizhang.API.Types.Record as ExpenseRequest (ExpenseRecordRequest (..))
import qualified Jizhang.API.Types.Record as StoredRecord (Record (..))
import qualified Jizhang.API.Types.Report as Report
import Jizhang.API.User (UserAPI)
import Jizhang.Currency (loadCurrencyCatalog)
import Jizhang.Database.Connection (closeConnectionManager, newConnectionManager)
import Jizhang.Database.Init (createTables, dropTables)
import qualified Jizhang.ExchangeRate as FX
import Jizhang.OIDC (newAuthRuntime)
import Jizhang.RequestProtection
import Log (Logger, runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, newManager, parseRequest)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp (withApplication)
import Servant hiding (addHeader)
import Servant.Auth (Auth, JWT)
import Servant.Auth.Client (Token (..))
import Servant.Auth.Server (IsMatch (Matches), JWTSettings (audienceMatches, validationKeys), defaultJWTSettings, generateKey, makeJWT, verifyJWT)
import Servant.Client
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

-- Servant client functions

-- Unprotected auth endpoints
register_ :: RegisterRequest -> ClientM User
login_ :: LoginRequest -> ClientM LoginResponse
(register_ :<|> login_) = client (Proxy :: Proxy AuthAPI)

adminLogin_ :: AdminLoginRequest -> ClientM AdminLoginResponse
adminLogin_ = loginClient
  where
    (loginClient :<|> _sessionLogin :<|> _sessionLogout :<|> _sessionMe) = client (Proxy :: Proxy AdminAuthAPI)

-- Protected endpoints (require JWT auth token)
type TestProtectedAPI =
  Auth '[JWT] AuthUser :> (UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ReceiptAPI)

type TestProtectedAdminAPI =
  Auth '[JWT] AuthAdmin :> AdminAPI

protectedClient_ ::
  Token ->
  Client ClientM (UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ReceiptAPI)
protectedClient_ = client (Proxy :: Proxy TestProtectedAPI)

protectedAdminClient_ :: Token -> Client ClientM AdminAPI
protectedAdminClient_ = client (Proxy :: Proxy TestProtectedAdminAPI)

-- | Token-bound clients for the user API; group shortcuts use single-currency USD.
data TC = TC
  { cGetMe :: ClientM UserProfile,
    cDeleteMe :: ClientM NoContent,
    cGetMyGroups :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Group),
    cUpdateMe :: UpdateProfileRequest -> ClientM UserProfile,
    cCreateGroup :: Text -> ClientM Group,
    cGetGroupById :: GroupId -> ClientM Group,
    cUpdateGroupById :: GroupId -> Text -> ClientM Group,
    cDeleteGroupById :: GroupId -> ClientM NoContent,
    cAddMember :: GroupId -> UserId -> ClientM Group,
    cDeleteMember :: GroupId -> UserId -> ClientM NoContent,
    cTransferOwnership :: GroupId -> UserId -> ClientM Group,
    cImportGroupCsv :: GroupId -> CSVData -> ClientM [Record],
    cMemberCandidates :: GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse User),
    cAddExpense :: GroupId -> ExpenseRecordRequest -> ClientM Record,
    cAddTransfer :: GroupId -> TransferRecordRequest -> ClientM Record,
    cGetRecord :: GroupId -> RecordId -> ClientM Record,
    cGetRecords :: GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Record),
    cDeleteRecord :: GroupId -> RecordId -> ClientM NoContent,
    cUpdateTransfer :: GroupId -> RecordId -> TransferRecordRequest -> ClientM Record,
    cUpdateExpense :: GroupId -> RecordId -> ExpenseRecordRequest -> ClientM Record,
    cGetReport :: GroupId -> ClientM Report,
    cCreateReceipt :: GroupId -> CreateReceiptRequest -> ClientM Receipt,
    cGetReceipts :: GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Receipt),
    cGetReceipt :: GroupId -> ReceiptId -> ClientM Receipt,
    cDeleteReceipt :: GroupId -> ReceiptId -> ClientM NoContent,
    cUpdateReceipt :: GroupId -> ReceiptId -> UpdateReceiptRequest -> ClientM Receipt,
    cExchangeQuote :: GroupId -> ExchangeQuoteRequest -> ClientM ExchangeQuote,
    cGetStatistics :: GroupId -> StatsDate -> StatsDate -> Maybe StatsBucket -> Maybe UserId -> ClientM SpendingStatistics,
    cAddIncome :: GroupId -> IncomeRecordRequest -> ClientM Record,
    cUpdateIncome :: GroupId -> RecordId -> IncomeRecordRequest -> ClientM Record
  }

-- | Administrator clients, including cross-group operations unavailable to ordinary members.
data AdminTC = AdminTC
  { cAdminUsers :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Oidc.AdminUserSummary),
    cAdminCreateUser :: AdminCreateUserRequest -> ClientM Oidc.AdminUserDetails,
    cAdminBulkDeleteUsers :: BulkRequest UserId -> ClientM NoContent,
    cAdminDeleteUser :: UserId -> ClientM NoContent,
    cAdminAdmins :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse AdminSummary),
    cAdminCreateAdmin :: AdminCreateAdminRequest -> ClientM AdminSummary,
    cAdminGroups :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Group),
    cAdminCreateGroup :: AdminCreateGroupRequest -> ClientM Group,
    cAdminBulkDeleteGroups :: BulkRequest GroupId -> ClientM NoContent,
    cAdminGroup :: GroupId -> ClientM Group,
    cAdminUpdateGroup :: GroupId -> Text -> ClientM Group,
    cAdminDeleteGroup :: GroupId -> ClientM NoContent,
    cAdminAddMember :: GroupId -> UserId -> ClientM Group,
    cAdminBulkAddMembers :: GroupId -> BulkRequest UserId -> ClientM Group,
    cAdminDeleteMember :: GroupId -> UserId -> ClientM NoContent,
    cAdminBulkDeleteMembers :: GroupId -> BulkRequest UserId -> ClientM NoContent,
    cAdminTransferOwnership :: GroupId -> UserId -> ClientM Group,
    cAdminImportGroupCsv :: GroupId -> CSVData -> ClientM [Record],
    cAdminAddExpense :: GroupId -> ExpenseRecordRequest -> ClientM Record,
    cAdminAddTransfer :: GroupId -> TransferRecordRequest -> ClientM Record,
    cAdminGroupRecords :: GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Record),
    cAdminBulkDeleteRecords :: GroupId -> BulkRequest RecordId -> ClientM NoContent,
    cAdminDeleteRecord :: GroupId -> RecordId -> ClientM NoContent,
    cAdminUpdateTransfer :: GroupId -> RecordId -> TransferRecordRequest -> ClientM Record,
    cAdminUpdateExpense :: GroupId -> RecordId -> ExpenseRecordRequest -> ClientM Record,
    cAdminGroupReport :: GroupId -> ClientM Report,
    cAdminCreateReceipt :: GroupId -> AdminCreateReceiptRequest -> ClientM Receipt,
    cAdminGroupReceipts :: GroupId -> ClientM [Receipt],
    cAdminBulkDeleteReceipts :: GroupId -> BulkRequest ReceiptId -> ClientM NoContent,
    cAdminDeleteReceipt :: GroupId -> ReceiptId -> ClientM NoContent,
    cAdminUpdateReceipt :: GroupId -> ReceiptId -> UpdateReceiptRequest -> ClientM Receipt,
    cAdminExchangeQuote :: GroupId -> ExchangeQuoteRequest -> ClientM ExchangeQuote,
    cAdminStatistics :: GroupId -> StatsDate -> StatsDate -> Maybe StatsBucket -> Maybe UserId -> ClientM SpendingStatistics,
    cAdminAddIncome :: GroupId -> IncomeRecordRequest -> ClientM Record,
    cAdminUpdateIncome :: GroupId -> RecordId -> IncomeRecordRequest -> ClientM Record,
    cAdminGroupRecord :: GroupId -> RecordId -> ClientM Record
  }

-- | Bind a bearer token; group creation, updates and CSV imports default to USD.
mkTC :: Text -> TC
mkTC tok =
  let auth = Token (encodeUtf8 tok)
      (uc :<|> gc :<|> rc :<|> rpt :<|> rcp) = protectedClient_ auth
      (a1 :<|> a2 :<|> a3 :<|> a4 :<|> _accountAuthentication) = uc
      (b1 :<|> b2 :<|> b3 :<|> b4 :<|> b5 :<|> b6 :<|> b7 :<|> b8 :<|> b9 :<|> b10) = gc
      (c1 :<|> c2 :<|> c3 :<|> c4 :<|> c5 :<|> c6 :<|> c7 :<|> c8 :<|> c9) = rc
      (d1 :<|> d2 :<|> d3 :<|> d4 :<|> d5) = rcp
      (report :<|> statistics) = rpt
   in TC a1 a2 a3 a4 (\name -> b1 (CreateGroupRequest name SingleCurrency (CurrencyCode "USD"))) b2 (\gid name -> b3 gid (UpdateGroupRequest name SingleCurrency (CurrencyCode "USD"))) b4 b5 b6 b7 (\gid -> b8 gid (CurrencyCode "USD") Nothing Nothing) b9 c1 c2 c3 c4 c5 c6 c7 report d1 d2 d3 d4 d5 b10 statistics c8 c9

-- | Bind an admin token; update and CSV shortcuts retain the USD fixture convention.
mkAdminTC :: Text -> AdminTC
mkAdminTC tok =
  let auth = Token (encodeUtf8 tok)
      (a1 :<|> a2 :<|> a3 :<|> a4 :<|> a5 :<|> a6 :<|> a7 :<|> a8 :<|> a9 :<|> a10 :<|> a11 :<|> a12 :<|> a13 :<|> a14 :<|> a15 :<|> a16 :<|> a17 :<|> a18 :<|> a19 :<|> a20 :<|> a21 :<|> a22 :<|> a23 :<|> a24 :<|> a25 :<|> a26 :<|> a27 :<|> a28 :<|> a29 :<|> a30 :<|> a31 :<|> a32 :<|> a33 :<|> a34 :<|> a35 :<|> a36 :<|> _accountManagement) = protectedAdminClient_ auth
   in AdminTC a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 (\gid name -> a11 gid (UpdateGroupRequest name SingleCurrency (CurrencyCode "USD"))) a12 a13 a14 a15 a16 a17 (\gid -> a18 gid (CurrencyCode "USD") Nothing Nothing) a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36

-- The control connection stays separate from the application's managed connection.

withTestApp :: (ClientEnv -> IO a) -> IO a
withTestApp = withTestAppWithBootstrap Nothing

withTestAppWithBootstrap :: Maybe AdminBootstrap -> (ClientEnv -> IO a) -> IO a
withTestAppWithBootstrap adminBootstrap action = withTestAppServer adminBootstrap id (\env _ _ _ -> action env)

-- | Start Warp on an allocated port and expose both HTTP and direct SQL access.
-- Setup and teardown drop tables, so this fixture requires the named disposable database.
withTestAppServer :: Maybe AdminBootstrap -> (AppEnv -> AppEnv) -> (ClientEnv -> Int -> Manager -> Connection -> IO a) -> IO a
withTestAppServer adminBootstrap configure action = withStdOutLogger $ \logger ->
  withTestAppServerLogger logger adminBootstrap configure action

-- | Use a supplied logger without changing the destructive database lifecycle.
withTestAppServerLogger :: Logger -> Maybe AdminBootstrap -> (AppEnv -> AppEnv) -> (ClientEnv -> Int -> Manager -> Connection -> IO a) -> IO a
withTestAppServerLogger logger adminBootstrap configure action =
  withTestAppServerLoggerEnv logger adminBootstrap configure (\_ -> action)

-- | Also expose the configured environment for connection and runtime assertions.
-- Each run gets fresh JWT keys; reference-rate fetching is unavailable unless overridden.
withTestAppServerLoggerEnv :: Logger -> Maybe AdminBootstrap -> (AppEnv -> AppEnv) -> (AppEnv -> ClientEnv -> Int -> Manager -> Connection -> IO a) -> IO a
withTestAppServerLoggerEnv logger adminBootstrap configure action = do
  connStr <- fromMaybe "dbname=jizhang_test" . fmap pack <$> lookupEnv "TEST_DATABASE_URL"
  bracket (connectPostgreSQL connStr) close $ \conn -> do
    database <- query_ conn "SELECT current_database()" :: IO [Only Text]
    assertEqual "integration tests require the disposable database" [Only "jizhang_test"] database
    bracket_ ((dropTables conn >> createTables conn) `onException` dropTables conn) (dropTables conn) $
      bracket (newConnectionManager connStr) closeConnectionManager $ \connectionManager ->
        withManagedApp connectionManager conn
  where
    withManagedApp connectionManager conn = do
      jwk <- generateKey
      currencies <- loadCurrencyCatalog
      authRuntime <- newAuthRuntime Nothing Nothing Nothing []
      let generousRate = RatePolicy 1000000 1000000
      protection <-
        newRequestProtection
          defaultRequestProtectionConfig
            { protectionIpRate = generousRate,
              protectionAuthIpRate = generousRate,
              protectionPrincipalRate = generousRate,
              protectionExpensiveRate = generousRate,
              protectionCredentialRate = generousRate
            }
          getMonotonicTimeNSec
      let appEnv =
            configure
              AppEnv
                { appConn = connectionManager,
                  appJWTSettings = defaultJWTSettings jwk,
                  appAuthRuntime = authRuntime,
                  appRequestProtection = protection,
                  appAdminBootstrap = adminBootstrap,
                  appEnvironmentAuthentication = Nothing,
                  appAdminCookieSecure = False,
                  appClientOrigins = [],
                  appCurrencies = currencies,
                  appFetchReferenceRate = \_ _ -> pure (Left FX.ProviderUnavailable)
                }
      waiApp <- runLogT "test" logger maxBound $ app appEnv
      manager <- newManager defaultManagerSettings
      withApplication (pure waiApp) $ \port -> do
        let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
        action appEnv env port manager conn

-- | Require client success; transport, decoding and HTTP failures all fail the assertion.
runClient :: ClientEnv -> ClientM a -> IO a
runClient env action = do
  result <- runClientM action env
  case result of
    Left err -> assertFailure ("Client error: " ++ show err) >> undefined
    Right a -> pure a

-- | Require an HTTP failure with this status; transport or decoding errors do not qualify.
runClientExpectStatus :: ClientEnv -> Status -> ClientM a -> IO ()
runClientExpectStatus env expectedStatus action = do
  result <- runClientM action env
  case result of
    Left (FailureResponse _ resp) ->
      assertEqual "status code" expectedStatus (responseStatusCode resp)
    Left err -> assertFailure ("Expected FailureResponse but got: " ++ show err)
    Right _ -> assertFailure "Expected failure but got success"

-- | Whole-dollar fixtures, converted exactly to the explicit USD wire contract.
usdExpense :: Text -> Integer -> UserId -> Day -> [RecordSplitRequest] -> ExpenseRecordRequest
usdExpense title dollars payer day splits =
  ExpenseRecordRequest title Nothing (MinorAmount (dollars * 100)) (CurrencyCode "USD") payer day SharesSplit splits Nothing

-- | Express whole-dollar transfers without introducing decimal or floating-point fixtures.
usdTransfer :: Integer -> UserId -> UserId -> Day -> TransferRecordRequest
usdTransfer dollars payer recipient day =
  TransferRecordRequest (MinorAmount (dollars * 100)) (CurrencyCode "USD") payer recipient day Nothing

assertMoneyMajor :: String -> Integer -> MinorAmount -> Assertion
assertMoneyMajor message dollars = assertEqual message (MinorAmount (dollars * 100))

-- | Register a unique fixture name with the shared password and return its login token.
setupUser :: ClientEnv -> Text -> IO Text
setupUser env name = do
  _ <- runClient env $ register_ (RegisterRequest name name (name <> "@example.test") "password123")
  resp <- runClient env $ login_ (LoginRequest name "password123")
  pure resp.accessToken

loginWithPassword :: ClientEnv -> Text -> Text -> IO Text
loginWithPassword env name password = do
  resp <- runClient env $ login_ (LoginRequest name password)
  pure resp.accessToken

-- | Log in an already bootstrapped administrator; this helper does not create one.
setupAdmin :: ClientEnv -> Text -> Text -> IO Text
setupAdmin env name password = do
  resp <- runClient env $ adminLogin_ (AdminLoginRequest name password)
  pure resp.accessToken

-- | Resolve an existing shared-password fixture through login, not a database lookup.
fixtureUserId :: ClientEnv -> Text -> IO UserId
fixtureUserId env name = do
  response <- runClient env $ login_ (LoginRequest name "password123")
  pure $ userIdOf $ loginUser response

profileUser :: UserProfile -> User
profileUser (UserProfile value _ _) = value

userIdOf :: User -> UserId
userIdOf (User value _ _) = value

-- Positional projections avoid ambiguity between identically named response fields.

-- | Extract username text from a User (avoids DuplicateRecordFields ambiguity)
userName :: User -> Text
userName (User _ n _) = n

grpId :: Group -> GroupId
grpId Group {groupId = gid} = gid

grpName :: Group -> Text
grpName Group {groupName = name} = name

grpOwnerName :: Group -> Text
grpOwnerName Group {owner = value} = userName value

grpMemberNames :: Group -> [Text]
grpMemberNames Group {members = values} = sort $ map userName values

titleOfRecord :: Record -> Text
titleOfRecord ExpenseRecord {title = t} = t
titleOfRecord TransferRecord {title = t} = t
titleOfRecord IncomeRecord {title = t} = t

adminSummaryUsername :: AdminSummary -> Text
adminSummaryUsername (AdminSummary _ uname) = uname

pageItems :: PaginatedResponse a -> [a]
pageItems (PaginatedResponse xs _ _ _) = xs

pageMeta :: PaginatedResponse a -> PageInfo
pageMeta (PaginatedResponse _ meta _ _) = meta

withPagination :: (Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM a) -> PaginationParams -> ClientM a
withPagination clientCall params =
  let (mQuery, mOffset, mLimit, mSort) = toPaginationArgs params
   in clientCall mQuery mOffset mLimit mSort

withPagination1 :: (a -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM b) -> a -> PaginationParams -> ClientM b
withPagination1 clientCall arg params =
  let (mQuery, mOffset, mLimit, mSort) = toPaginationArgs params
   in clientCall arg mQuery mOffset mLimit mSort

-- Traverse every page for assertions about the complete ledger.
allAdminRecords :: AdminTC -> GroupId -> ClientM [Record]
allAdminRecords admin gid = go 0
  where
    go offset = do
      PaginatedResponse records (PageInfo _ limit _ next _) _ _ <-
        cAdminGroupRecords admin gid Nothing (Just offset) (Just 100) Nothing
      remaining <- if next then go (offset + limit) else pure []
      pure $ records ++ remaining

extractRecordId :: Record -> RecordId
extractRecordId ExpenseRecord {..} = recordId
extractRecordId TransferRecord {..} = recordId
extractRecordId IncomeRecord {..} = recordId

receiptNote :: Receipt -> Text
receiptNote (Receipt _ _ _ n _ _) = n

receiptRecords :: Receipt -> [Record]
receiptRecords (Receipt _ _ _ _ rs _) = rs

-- Shared integration groups use the same destructive fixture rather than a live deployment.

-- | Exercise public HTTP contracts alongside persisted state and permission boundaries.
apiTests :: TestTree
apiTests =
  testGroup
    "API Integration"
    [ authTests,
      currencyApiTests,
      currencyPolicyTests,
      identityTests,
      adminTests,
      adminUiTests,
      browserTransportTests,
      userTests,
      groupTests,
      memberCandidateTests,
      recordTests,
      sharedIncomeTests,
      reportIntegrationTests,
      authorizationTests,
      receiptTests,
      integrityTests
    ]

currencyApiTests :: TestTree
currencyApiTests =
  testGroup
    "Currency"
    [ testCase "catalogue is public and permits only configured browser origins" $
        withTestAppServer Nothing (\env -> env {appClientOrigins = ["http://client.example"]}) $ \_ port manager _ -> do
          req <- localRequest port "/currencies"
          response <- httpLbs (req {HC.requestHeaders = [("Origin", "http://client.example")]}) manager
          HC.responseStatus response @?= status200
          lookup "Access-Control-Allow-Origin" (HC.responseHeaders response) @?= Just "http://client.example"
          catalog <- loadCurrencyCatalog
          case eitherDecode (HC.responseBody response) of
            Left err -> assertFailure err
            Right currencies -> currencies @?= M.elems catalog
          rejected <- httpLbs (req {HC.requestHeaders = [("Origin", "http://untrusted.example")]}) manager
          lookup "Access-Control-Allow-Origin" (HC.responseHeaders rejected) @?= Nothing
    ]

-- Auth tests
authTests :: TestTree
authTests =
  testGroup
    "Auth"
    [ testCase "register and login" $ withTestApp $ \env -> do
        u <- runClient env $ register_ (RegisterRequest "alice" "alice" ("alice" <> "@example.test") "password123")
        assertEqual "username" "alice" (userName u)
        resp <- runClient env $ login_ (LoginRequest "alice" "password123")
        assertEqual "login user" "alice" (userName $ loginUser resp)
        profile <- runClient env $ cGetMe (mkTC resp.accessToken)
        assertEqual "protected session owner" (userIdOf u) (userIdOf $ profileUser profile),
      --
      testCase "register duplicate => 409" $ withTestApp $ \env -> do
        _ <- runClient env $ register_ (RegisterRequest "alice" "alice" ("alice" <> "@example.test") "password123")
        runClientExpectStatus env status409 $ register_ (RegisterRequest "alice" "alice" ("alice" <> "@example.test") "password123"),
      --
      testCase "login wrong password => 401" $ withTestApp $ \env -> do
        _ <- runClient env $ register_ (RegisterRequest "alice" "alice" ("alice" <> "@example.test") "password123")
        runClientExpectStatus env status401 $ login_ (LoginRequest "alice" "wrongpassword"),
      --
      testCase "login nonexistent user => 401" $ withTestApp $ \env -> do
        runClientExpectStatus env status401 $ login_ (LoginRequest "nobody" "password123"),
      --
      testCase "protected endpoint without token => 401" $ withTestApp $ \env -> do
        let tc = mkTC "invalid-token"
        runClientExpectStatus env status401 (cGetMe tc),
      testCase "password limit uses UTF-8 bytes without truncation" $ withTestApp $ \env -> do
        let atLimit = T.replicate 24 "会"
        _ <- runClient env $ register_ (RegisterRequest "alice" "Alice" "alice@example.test" atLimit)
        session <- runClient env $ login_ (LoginRequest "alice" atLimit)
        profile <- runClient env $ cGetMe (mkTC session.accessToken)
        assertEqual "valid multibyte password" "alice" (userName $ profileUser profile)
        runClientExpectStatus env status400 $ register_ (RegisterRequest "bob" "Bob" "bob@example.test" (atLimit <> "a"))
        runClientExpectStatus env status401 $ login_ (LoginRequest "bob" (atLimit <> "a"))
    ]

adminTests :: TestTree
adminTests =
  testGroup
    "Admin"
    [ testCase "bootstrap admin can list all users" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        _ <- setupUser env "bob"
        let tcAdmin = mkAdminTC tokAdmin
        users <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams
        assertEqual "usernames" ["bob"] (sort $ map (\summary -> userName $ profileUser summary.profile) $ pageItems users),
      testCase "app user token cannot access admin endpoints => 401" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokBob <- setupUser env "bob"
        runClientExpectStatus env status401 $ cAdminUsers (mkAdminTC tokBob) Nothing Nothing Nothing Nothing,
      testCase "admin can create users and admins" $ withTestAppWithBootstrap (Just $ AdminBootstrap "rootadmin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "rootadmin" "adminpass123"
        let tcAdmin = mkAdminTC tokAdmin
        createdUser <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "carol" "carol" ("carol" <> "@example.test") AdminCreateLocal (Just "password123"))
        assertEqual "created user" "carol" (userName $ profileUser createdUser.profile)
        createdAdmin <- runClient env $ cAdminCreateAdmin tcAdmin (AdminCreateAdminRequest "ops" "password123")
        assertEqual "created admin" "ops" (adminSummaryUsername createdAdmin)
        tokOps <- setupAdmin env "ops" "password123"
        let tcOps = mkAdminTC tokOps
        admins <- runClient env $ cAdminAdmins tcOps Nothing Nothing Nothing Nothing
        assertEqual "admin usernames" ["ops", "rootadmin"] (sort $ map adminSummaryUsername $ pageItems admins),
      testCase "admin list endpoints support query filters" $ withTestAppWithBootstrap (Just $ AdminBootstrap "rootadmin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "rootadmin" "adminpass123"
        let tcAdmin = mkAdminTC tokAdmin
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "alice" "alice" ("alice" <> "@example.test") AdminCreateLocal (Just "password123"))
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "bob" "bob" ("bob" <> "@example.test") AdminCreateLocal (Just "password123"))
        _ <- runClient env $ cAdminCreateAdmin tcAdmin (AdminCreateAdminRequest "ops-team" "password123")
        tokAlice <- loginWithPassword env "alice" "password123"
        let tcAlice = mkTC tokAlice
        _ <- runClient env $ cCreateGroup tcAlice "Summer Trip"
        _ <- runClient env $ cCreateGroup tcAlice "Work Budget"
        users <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationQuery = Just "ali"}
        assertEqual "filtered users" ["alice"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems users)
        admins <- runClient env $ withPagination (cAdminAdmins tcAdmin) defaultPaginationParams {paginationQuery = Just "ops"}
        assertEqual "filtered admins" ["ops-team"] (map adminSummaryUsername $ pageItems admins)
        groups <- runClient env $ withPagination (cAdminGroups tcAdmin) defaultPaginationParams {paginationQuery = Just "trip"}
        assertEqual "filtered groups" ["Summer Trip"] (map grpName $ pageItems groups),
      testCase "admin list endpoints support pagination and sorting" $ withTestAppWithBootstrap (Just $ AdminBootstrap "rootadmin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "rootadmin" "adminpass123"
        let tcAdmin = mkAdminTC tokAdmin
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "charlie" "charlie" ("charlie" <> "@example.test") AdminCreateLocal (Just "password123"))
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "alice" "alice" ("alice" <> "@example.test") AdminCreateLocal (Just "password123"))
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "bob" "bob" ("bob" <> "@example.test") AdminCreateLocal (Just "password123"))
        page1 <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationOffset = Just 0, paginationLimit = Just 2, paginationSort = Just "username"}
        let PageInfo off1 lim1 total1 hasNext1 hasPrev1 = pageMeta page1
        assertEqual "page1 users" ["alice", "bob"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems page1)
        assertEqual "page1 offset" 0 off1
        assertEqual "page1 limit" 2 lim1
        assertEqual "page1 total" 3 total1
        assertEqual "page1 hasNext" True hasNext1
        assertEqual "page1 hasPrev" False hasPrev1
        page2 <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationOffset = Just 2, paginationLimit = Just 2, paginationSort = Just "username"}
        assertEqual "page2 users" ["charlie"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems page2)
        descPage <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationOffset = Just 0, paginationLimit = Just 3, paginationSort = Just "-username"}
        assertEqual "descending users" ["charlie", "bob", "alice"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems descPage),
      testCase "admin can inspect groups without membership" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        _ <- runClient env $ cAddExpense tcAlice (grpId g) (usdExpense "Dinner" 100 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))])
        groups <- runClient env $ cAdminGroups tcAdmin Nothing Nothing Nothing Nothing
        assertEqual "group names" ["Trip"] (map grpName $ pageItems groups)
        fetched <- runClient env $ cAdminGroup tcAdmin (grpId g)
        assertEqual "fetched group name" "Trip" (grpName fetched)
        records <- runClient env $ allAdminRecords tcAdmin (grpId g)
        assertEqual "record count" 1 (length records)
        report <- runClient env $ cAdminGroupReport tcAdmin (grpId g)
        assertEqual "settlement count" 1 (length $ concatMap settlements (Report.currencies report))
        receipts <- runClient env $ cAdminGroupReceipts tcAdmin (grpId g)
        assertEqual "receipt count" 0 (length receipts),
      testCase "admin can update group and membership without ownership" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        g1 <- runClient env $ cAdminUpdateGroup tcAdmin (grpId g) "Renamed Trip"
        assertEqual "updated name" "Renamed Trip" (grpName g1)
        g2 <- runClient env $ cAdminAddMember tcAdmin (grpId g) bobId
        assertEqual "members after add" ["alice", "bob"] (sort $ grpMemberNames g2)
        _ <- runClient env $ cAdminTransferOwnership tcAdmin (grpId g) bobId
        fetched <- runClient env $ cAdminGroup tcAdmin (grpId g)
        assertEqual "new owner" "bob" (grpOwnerName fetched)
        _ <- runClient env $ cAdminDeleteMember tcAdmin (grpId g) aliceId
        fetched2 <- runClient env $ cAdminGroup tcAdmin (grpId g)
        assertEqual "members after delete" ["bob"] (grpMemberNames fetched2),
      testCase "admin can update and delete records" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        expense <- runClient env $ cAdminAddExpense tcAdmin (grpId g) (usdExpense "Dinner" 100 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))])
        expense' <- runClient env $ cAdminUpdateExpense tcAdmin (grpId g) (extractRecordId expense) (usdExpense "Lunch" 40 bobId (read "2025-06-02") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))])
        case expense' of
          ExpenseRecord {title = recordTitle, amountMinor = recordAmount, paidBy = payer} -> do
            assertEqual "updated title" "Lunch" recordTitle
            assertMoneyMajor "updated amount" 40 recordAmount
            assertEqual "updated payer" "bob" (userName payer)
          _ -> assertFailure "Expected updated expense record"
        transfer <- runClient env $ cAdminAddTransfer tcAdmin (grpId g) (usdTransfer (10) (aliceId) (bobId) ((read "2025-06-03")))
        transfer' <- runClient env $ cAdminUpdateTransfer tcAdmin (grpId g) (extractRecordId transfer) (usdTransfer (25) (bobId) (aliceId) ((read "2025-06-04")))
        case transfer' of
          TransferRecord {amountMinor = transferAmount, paidBy = payer, transferTo = receiver} -> do
            assertMoneyMajor "updated transfer amount" 25 transferAmount
            assertEqual "updated transfer payer" "bob" (userName payer)
            assertEqual "updated transfer receiver" "alice" (userName receiver)
          _ -> assertFailure "Expected updated transfer record"
        _ <- runClient env $ cAdminDeleteRecord tcAdmin (grpId g) (extractRecordId transfer)
        runClientExpectStatus env status404 $ cGetRecord tcAlice (grpId g) (extractRecordId transfer),
      testCase "admin can update and delete receipts" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        receipt <- runClient env $ cAdminCreateReceipt tcAdmin (grpId g) (AdminCreateReceiptRequest aliceId "Original" [usdExpense "Dinner" 100 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]])
        receipt' <- runClient env $ cAdminUpdateReceipt tcAdmin (grpId g) (receiptId receipt) (UpdateReceiptRequest "Updated" [usdExpense "Taxi" 60 bobId (read "2025-06-02") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]])
        let Receipt _ _ _ updatedNote updatedRecords _ = receipt'
        assertEqual "updated receipt note" "Updated" updatedNote
        assertEqual "updated receipt record title" ["Taxi"] (map titleOfRecord updatedRecords)
        _ <- runClient env $ cAdminDeleteReceipt tcAdmin (grpId g) (receiptId receipt)
        runClientExpectStatus env status404 $ cGetReceipt tcAlice (grpId g) (receiptId receipt),
      testCase "admin can delete users" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        alice <- runClient env $ cGetMe tcAlice
        _ <- runClient env $ cAdminDeleteUser tcAdmin (userIdOf (profileUser alice))
        runClientExpectStatus env status401 $ cGetMe tcAlice,
      testCase "admin bulk operations work" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        _ <- setupUser env "carol"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        bobPage <- runClient env $ cAdminUsers tcAdmin (Just "bob") Nothing Nothing Nothing
        carolPage <- runClient env $ cAdminUsers tcAdmin (Just "carol") Nothing Nothing Nothing
        bobUser <- case pageItems bobPage of
          [u] -> pure u
          xs -> assertFailure ("Expected exactly one bob user, got: " <> show (length xs)) >> undefined
        carolUser <- case pageItems carolPage of
          [u] -> pure u
          xs -> assertFailure ("Expected exactly one carol user, got: " <> show (length xs)) >> undefined
        _ <- runClient env $ cAdminBulkDeleteUsers tcAdmin (BulkRequest [userIdOf (profileUser bobUser.profile), userIdOf (profileUser carolUser.profile)])
        usersAfterDelete <- runClient env $ cAdminUsers tcAdmin Nothing Nothing Nothing (Just "username")
        assertEqual "users after bulk delete" ["alice"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems usersAfterDelete)
        g1 <- runClient env $ cCreateGroup tcAlice "Trip"
        g2 <- runClient env $ cCreateGroup tcAlice "Work"
        g3 <- runClient env $ cCreateGroup tcAlice "Archive"
        _ <- runClient env $ cAdminBulkDeleteGroups tcAdmin (BulkRequest [grpId g2])
        _ <- runClient env $ cAdminDeleteGroup tcAdmin (grpId g3)
        groupsAfterDelete <- runClient env $ cAdminGroups tcAdmin Nothing Nothing Nothing (Just "groupName")
        assertEqual "remaining groups" ["Trip"] (map grpName $ pageItems groupsAfterDelete)
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "dave" "dave" ("dave" <> "@example.test") AdminCreateLocal (Just "password123"))
        daveId <- fixtureUserId env "dave"
        _ <- runClient env $ cAdminBulkAddMembers tcAdmin (grpId g1) (BulkRequest [daveId])
        fetched1 <- runClient env $ cAdminGroup tcAdmin (grpId g1)
        assertEqual "members after bulk add" ["alice", "dave"] (grpMemberNames fetched1)
        _ <- runClient env $ cAdminBulkDeleteMembers tcAdmin (grpId g1) (BulkRequest [daveId])
        fetched2 <- runClient env $ cAdminGroup tcAdmin (grpId g1)
        assertEqual "members after bulk delete" ["alice"] (grpMemberNames fetched2)
        _ <- runClient env $ cAdminBulkAddMembers tcAdmin (grpId g1) (BulkRequest [daveId])
        expense <- runClient env $ cAddExpense tcAlice (grpId g1) (usdExpense "Dinner" 100 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (daveId) (MinorAmount (1))])
        transfer <- runClient env $ cAddTransfer tcAlice (grpId g1) (usdTransfer (15) (daveId) (aliceId) ((read "2025-06-02")))
        _ <- runClient env $ cAdminBulkDeleteRecords tcAdmin (grpId g1) (BulkRequest [extractRecordId expense, extractRecordId transfer])
        recordsAfterDelete <- runClient env $ allAdminRecords tcAdmin (grpId g1)
        assertEqual "records after bulk delete" 0 (length recordsAfterDelete)
        receipt <- runClient env $ cCreateReceipt tcAlice (grpId g1) (CreateReceiptRequest "Original" [usdExpense "Taxi" 60 aliceId (read "2025-06-03") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (daveId) (MinorAmount (1))]])
        _ <- runClient env $ cAdminBulkDeleteReceipts tcAdmin (grpId g1) (BulkRequest [receiptId receipt])
        receiptsAfterDelete <- runClient env $ cAdminGroupReceipts tcAdmin (grpId g1)
        assertEqual "receipts after bulk delete" 0 (length receiptsAfterDelete)
    ]

adminUiTests :: TestTree
adminUiTests =
  testGroup
    "Admin UI"
    [ testCase "admin session cookie can access admin APIs" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \_ port manager _ -> do
        loginResp <- loginAdminSession port manager
        jwtCookie <- sessionCookie "JWT-Cookie" loginResp
        xsrfCookie <- sessionCookie "XSRF-TOKEN" loginResp
        let cookieValue = "JWT-Cookie=" <> cookieValuePart jwtCookie <> "; XSRF-TOKEN=" <> cookieValuePart xsrfCookie
            headers = [("Cookie", cookieValue), ("X-XSRF-TOKEN", cookieValuePart xsrfCookie)]
        meReq <- localRequest port "/admin/auth/session/me"
        meResp <- httpLbs (meReq {HC.requestHeaders = headers}) manager
        assertEqual "session me status" status200 (HC.responseStatus meResp)
        assertEqual "session check does not refresh cookies" [] [value | (name, value) <- HC.responseHeaders meResp, name == "Set-Cookie"]
        usersReq <- localRequest port "/admin/users?offset=0&limit=20&sort=username"
        usersResp <- httpLbs (usersReq {HC.requestHeaders = headers}) manager
        assertEqual "admin users status" status200 (HC.responseStatus usersResp)
        assertEqual "admin API does not refresh cookies" [] [value | (name, value) <- HC.responseHeaders usersResp, name == "Set-Cookie"],
      testCase "operator cookie and bearer logins expire after 24 hours" $ withStdOutLogger $ \logger ->
        withTestAppServerLoggerEnv logger (Just $ AdminBootstrap "admin" "adminpass123") id $ \appEnv env port manager _ -> do
          before <- getCurrentTime
          loginResp <- loginAdminSession port manager
          jwtCookie <- sessionCookie "JWT-Cookie" loginResp
          bearer <- setupAdmin env "admin" "adminpass123"
          issuedAt <- getCurrentTime
          cookieExpiry <- tokenExpiry $ cookieValuePart jwtCookie
          bearerExpiry <- tokenExpiry $ encodeUtf8 bearer
          forM_ [("cookie", cookieExpiry), ("bearer", bearerExpiry)] $ \(label, expiry) ->
            assertBool
              (label <> " JWT deadline is 24 hours from login")
              (expiry >= addUTCTime (24 * 60 * 60 - 1) before && expiry <= addUTCTime (24 * 60 * 60) issuedAt)
          let jwtCookies = filter ((== "JWT-Cookie") . HC.cookie_name) $ HC.destroyCookieJar $ HC.responseCookieJar loginResp
          case jwtCookies of
            [cookie] -> assertBool "browser cookie and JWT share the deadline" (abs (realToFrac (diffUTCTime cookieExpiry (HC.cookie_expiry_time cookie)) :: Double) < 1)
            _ -> assertFailure "Expected one browser session cookie"
          verified <- verifyJWT (appJWTSettings appEnv) (cookieValuePart jwtCookie) :: IO (Maybe AuthAdmin)
          assertEqual "issued cookie verifies" (Just "admin") (authAdminUsername <$> verified)
          forM_ [("cookie", cookieValuePart jwtCookie), ("bearer", encodeUtf8 bearer)] $ \(label, token) -> do
            current <- verifyTokenAt (appJWTSettings appEnv) issuedAt token
            assertBool (label <> " verifies at issuance") $ either (const False) (const True) current
            expired <- verifyTokenAt (appJWTSettings appEnv) (addUTCTime (24 * 60 * 60 + 60) issuedAt) token
            case expired of
              Left JWT.JWTExpired -> pure ()
              _ -> assertFailure (label <> " must be rejected after 24 hours")
          forM_ ["/admin/auth/session/me", "/admin/users"] $ \path -> do
            req <- localRequest port path
            resp <- httpLbs (req {HC.requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 bearer)]}) manager
            assertEqual "bearer operator access" status200 (HC.responseStatus resp)
            assertEqual "bearer access cannot mint a nonexpiring cookie" [] [value | (name, value) <- HC.responseHeaders resp, name == "Set-Cookie"],
      testCase "legacy operator tokens without expiration cannot authenticate or clear cookies" $ withStdOutLogger $ \logger ->
        withTestAppServerLoggerEnv logger (Just $ AdminBootstrap "admin" "adminpass123") id $ \appEnv _ port manager _ -> do
          loginResp <- loginAdminSession port manager
          jwtCookie <- sessionCookie "JWT-Cookie" loginResp
          xsrfCookie <- sessionCookie "XSRF-TOKEN" loginResp
          current <- verifyJWT (appJWTSettings appEnv) (cookieValuePart jwtCookie) :: IO (Maybe AuthAdmin)
          authAdmin <- maybe (assertFailure "Expiring operator token must verify" >> fail "Invalid operator token") pure current
          signed <- makeJWT authAdmin (appJWTSettings appEnv) Nothing
          legacy <- either (const $ assertFailure "Could not sign legacy operator token" >> fail "Invalid legacy fixture") (pure . LBS8.toStrict) signed
          now <- getCurrentTime
          verifiedClaims <- verifyTokenAt (appJWTSettings appEnv) now legacy
          assertBool "legacy fixture has a valid signature" $ either (const False) (const True) verifiedClaims
          rejected <- verifyJWT (appJWTSettings appEnv) legacy :: IO (Maybe AuthAdmin)
          assertEqual "operator decoder requires standard expiration" Nothing rejected
          forM_
            [ ("bearer", [("Authorization", "Bearer " <> legacy)]),
              ("cookie", [("Cookie", "JWT-Cookie=" <> legacy <> "; XSRF-TOKEN=" <> cookieValuePart xsrfCookie), ("X-XSRF-TOKEN", cookieValuePart xsrfCookie)])
            ]
            $ \(label, headers) -> do
              forM_ ["/admin/auth/session/me", "/admin/users"] $ \path -> do
                req <- localRequest port path
                resp <- httpLbs (req {HC.cookieJar = Nothing, HC.requestHeaders = headers}) manager
                assertEqual (label <> " legacy token is unauthorized") status401 (HC.responseStatus resp)
                assertEqual (label <> " legacy token cannot mint cookies") [] [value | (name, value) <- HC.responseHeaders resp, name == "Set-Cookie"]
              req <- localRequest port "/admin/auth/session/logout"
              resp <- httpLbs (req {HC.method = "POST", HC.cookieJar = Nothing, HC.requestHeaders = headers}) manager
              assertEqual (label <> " legacy logout is an idempotent no-op") status200 (HC.responseStatus resp)
              assertEqual (label <> " legacy token cannot delete cookies") [] [value | (name, value) <- HC.responseHeaders resp, name == "Set-Cookie"],
      testCase "operator document refuses framing without restricting assets" $ withTestAppServer Nothing id $ \_ port manager _ -> do
        req <- localRequest port "/admin-ui"
        resp <- httpLbs req manager
        assertEqual "operator document status" status200 (HC.responseStatus resp)
        assertEqual "frame ancestors policy" (Just "frame-ancestors 'none'") (lookup "Content-Security-Policy" $ HC.responseHeaders resp)
        assertEqual "legacy framing denial" (Just "DENY") (lookup "X-Frame-Options" $ HC.responseHeaders resp)
        forM_ ["/admin-ui/styles.css", "/admin-ui/app.js", "/currencies"] $ \path -> do
          asset <- localRequest port path >>= flip httpLbs manager
          assertEqual "asset or public API remains available" status200 (HC.responseStatus asset)
          assertEqual "document policy is not applied to other responses" Nothing (lookup "Content-Security-Policy" $ HC.responseHeaders asset),
      testCase "logout changes cookies only after successful authentication and XSRF" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        loginResp <- loginAdminSession port manager
        xsrfCookie <- sessionCookie "XSRF-TOKEN" loginResp
        let jar = HC.responseCookieJar loginResp
        req <- localRequest port "/admin/auth/session/logout"
        forM_
          [ ("anonymous", Nothing, []),
            ("invalid bearer", Nothing, [("Authorization", "Bearer invalid-token")]),
            ("form navigation", Nothing, [("Origin", "https://attacker.example.test"), ("Content-Type", "application/x-www-form-urlencoded")]),
            ("cookie without XSRF", Just jar, []),
            ("cookie with wrong XSRF", Just jar, [("X-XSRF-TOKEN", "incorrect-token")]),
            ("form with cookies", Just jar, [("Origin", "https://attacker.example.test"), ("Content-Type", "application/x-www-form-urlencoded")])
          ]
          $ \(label, cookies, headers) -> do
            resp <- httpLbs (req {HC.method = "POST", HC.cookieJar = cookies, HC.requestHeaders = headers, HC.requestBody = HC.RequestBodyBS ""}) manager
            assertEqual (label <> " is idempotently successful") status200 (HC.responseStatus resp)
            assertEqual (label <> " cannot set or delete cookies") [] [value | (name, value) <- HC.responseHeaders resp, name == "Set-Cookie"]
        meReq <- localRequest port "/admin/auth/session/me"
        stillSignedIn <- httpLbs (meReq {HC.cookieJar = Just jar, HC.requestHeaders = [("X-XSRF-TOKEN", cookieValuePart xsrfCookie)]}) manager
        assertEqual "rejected logout leaves the browser session usable" status200 (HC.responseStatus stillSignedIn)
        logoutResp <- httpLbs (req {HC.method = "POST", HC.cookieJar = Just jar, HC.requestHeaders = [("X-XSRF-TOKEN", cookieValuePart xsrfCookie)]}) manager
        assertEqual "authenticated logout status" status200 (HC.responseStatus logoutResp)
        forM_ ["JWT-Cookie", "XSRF-TOKEN"] $ \name -> do
          cookie <- sessionCookie name logoutResp
          assertBool "logout deletes rather than refreshes cookies" ("max-age=0" `elem` headerTokensBySemicolon cookie)
        signedOut <- httpLbs (meReq {HC.cookieJar = Just $ HC.responseCookieJar logoutResp, HC.requestHeaders = [("X-XSRF-TOKEN", cookieValuePart xsrfCookie)]}) manager
        assertEqual "cleared browser cookies no longer authenticate" status401 (HC.responseStatus signedOut)
        retry <- httpLbs (req {HC.method = "POST", HC.cookieJar = Just $ HC.responseCookieJar logoutResp}) manager
        assertEqual "retry succeeds" status200 (HC.responseStatus retry)
        assertEqual "retry emits no deletion cookies" [] [value | (name, value) <- HC.responseHeaders retry, name == "Set-Cookie"]
        bearer <- setupAdmin env "admin" "adminpass123"
        bearerLogout <- httpLbs (req {HC.method = "POST", HC.requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 bearer)]}) manager
        assertEqual "bearer logout does not require XSRF" status200 (HC.responseStatus bearerLogout)
        forM_ ["JWT-Cookie", "XSRF-TOKEN"] $ \name -> do
          cookie <- sessionCookie name bearerLogout
          assertBool "bearer logout emits deletion only" ("max-age=0" `elem` headerTokensBySemicolon cookie)
    ]

localRequest :: Int -> String -> IO HC.Request
localRequest port path = parseRequest $ "http://localhost:" <> show port <> path

loginAdminSession :: Int -> Manager -> IO (HC.Response LBS8.ByteString)
loginAdminSession port manager = do
  req <- localRequest port "/admin/auth/session/login"
  resp <- httpLbs (req {HC.method = "POST", HC.requestBody = HC.RequestBodyLBS "{\"username\":\"admin\",\"password\":\"adminpass123\"}", HC.requestHeaders = [("Content-Type", "application/json")]}) manager
  assertEqual "session login status" status200 (HC.responseStatus resp)
  pure resp

-- Return the cookie value and attributes, failing explicitly on missing/duplicate cookies.
sessionCookie :: B8.ByteString -> HC.Response body -> IO B8.ByteString
sessionCookie name resp =
  case [B8.drop (B8.length name + 1) value | (header, value) <- HC.responseHeaders resp, header == "Set-Cookie", (name <> "=") `B8.isPrefixOf` value] of
    [value] -> pure value
    values -> assertFailure ("Expected one " <> B8.unpack name <> " cookie, got " <> show (length values)) >> fail "Invalid session cookies"

cookieValuePart :: B8.ByteString -> B8.ByteString
cookieValuePart = B8.takeWhile (/= ';')

tokenExpiry :: B8.ByteString -> IO UTCTime
tokenExpiry token = case B8.split '.' token of
  [_, payload, _] -> case B64URL.decodeUnpadded payload >>= eitherDecode . LBS8.fromStrict of
    Right (Object claims) -> case KM.lookup "exp" claims of
      Just (Number seconds) -> pure $ posixSecondsToUTCTime $ realToFrac seconds
      _ -> assertFailure "JWT must contain a numeric expiration" >> fail "Missing JWT expiration"
    _ -> assertFailure "JWT claims must be a JSON object" >> fail "Invalid JWT claims"
  _ -> assertFailure "Expected a signed JWT" >> fail "Invalid JWT"

headerTokensBySemicolon :: B8.ByteString -> [B8.ByteString]
headerTokensBySemicolon = map (B8.map toLower . B8.dropWhile isSpace . B8.dropWhileEnd isSpace) . B8.split ';'

verifyTokenAt :: JWTSettings -> UTCTime -> B8.ByteString -> IO (Either JWT.JWTError JWT.ClaimsSet)
verifyTokenAt settings instant token = do
  keys <- validationKeys settings
  JWT.runJOSE $ do
    signed <- JWT.decodeCompact (LBS8.fromStrict token)
    JWT.verifyClaimsAt (JWT.defaultJWTValidationSettings matchesAudience) keys instant signed
  where
    matchesAudience audience = case audienceMatches settings audience of
      Matches -> True
      _ -> False

headerTokens :: B8.ByteString -> [B8.ByteString]
headerTokens = map (B8.map toLower . B8.dropWhile isSpace . B8.dropWhileEnd isSpace) . B8.split ','

browserTransportTests :: TestTree
browserTransportTests =
  testGroup
    "Browser transport"
    [ testCase "allowed-origin preflight permits bearer headers and PUT DELETE without credentials" $ withTestAppServer Nothing allowClient $ \_ port manager _ -> do
        resp <- preflight port manager "/groups" allowedOrigin
        assertEqual "preflight status" status200 (HC.responseStatus resp)
        assertAllowedOrigin resp
        let headers = HC.responseHeaders resp
            methods = maybe [] headerTokens $ lookup "Access-Control-Allow-Methods" headers
            requestHeaders = maybe [] headerTokens $ lookup "Access-Control-Allow-Headers" headers
        forM_ ["get", "post", "put", "delete", "options"] $ \method -> assertBool ("allowed method " <> B8.unpack method) (method `elem` methods)
        forM_ ["authorization", "content-type"] $ \header -> assertBool ("allowed header " <> B8.unpack header) (header `elem` requestHeaders),
      testCase "allowed-origin bearer request returns authenticated user" $ withTestAppServer Nothing allowClient $ \env port manager _ -> do
        tok <- setupUser env "alice"
        expected <- runClient env $ cGetMe (mkTC tok)
        req <- localRequest port "/users/me"
        resp <- httpLbs (req {HC.requestHeaders = [("Origin", allowedOrigin), ("Authorization", "Bearer " <> encodeUtf8 tok)]}) manager
        assertEqual "bearer request status" status200 (HC.responseStatus resp)
        assertAllowedOrigin resp
        assertEqual "authenticated identity" (Right expected) (eitherDecode $ HC.responseBody resp),
      testCase "different scheme host or port receives no cross-origin authorization" $ withTestAppServer Nothing allowClient $ \env port manager _ -> do
        tok <- setupUser env "alice"
        expected <- runClient env $ cGetMe (mkTC tok)
        forM_ ["https://127.0.0.1:3000", "http://localhost:3000", "http://127.0.0.1:3001"] $ \origin -> do
          optionsResp <- preflight port manager "/groups" origin
          assertNoOrigin optionsResp
          req <- localRequest port "/users/me"
          resp <- httpLbs (req {HC.requestHeaders = [("Origin", origin), ("Authorization", "Bearer " <> encodeUtf8 tok)]}) manager
          assertEqual "non-CORS bearer traffic still authorized" status200 (HC.responseStatus resp)
          assertEqual "authenticated identity" (Right expected) (eitherDecode $ HC.responseBody resp)
          assertNoOrigin resp,
      testCase "admin routes including session login never advertise CORS" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") allowClient $ \_ port manager _ -> do
        forM_ ["/admin/users", "/admin/auth/session/login", "/admin/auth/session/logout", "/admin/auth/session/me"] $ \path -> do
          resp <- preflight port manager path allowedOrigin
          assertNoOrigin resp
        req <- localRequest port "/admin/auth/session/login"
        resp <- httpLbs (req {HC.method = "POST", HC.requestBody = HC.RequestBodyLBS "{\"username\":\"admin\",\"password\":\"adminpass123\"}", HC.requestHeaders = [("Origin", allowedOrigin), ("Content-Type", "application/json")]}) manager
        assertEqual "same-origin admin endpoint remains available" status200 (HC.responseStatus resp)
        assertNoOrigin resp,
      testCase "empty origin allowlist disables CORS for preflight and bearer requests" $ withTestAppServer Nothing id $ \env port manager _ -> do
        optionsResp <- preflight port manager "/groups" allowedOrigin
        assertNoOrigin optionsResp
        tok <- setupUser env "alice"
        req <- localRequest port "/users/me"
        resp <- httpLbs (req {HC.requestHeaders = [("Origin", allowedOrigin), ("Authorization", "Bearer " <> encodeUtf8 tok)]}) manager
        assertEqual "bearer request status" status200 (HC.responseStatus resp)
        assertNoOrigin resp,
      testCase "secure mode marks both login and logout cookies Secure" $ checkCookieSecurity True,
      testCase "explicit development mode leaves both login and logout cookies non-Secure" $ checkCookieSecurity False,
      testCase "cookie admin mutations require matching XSRF and rejected requests write nothing" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        loginResp <- loginAdminSession port manager
        jwtCookie <- sessionCookie "JWT-Cookie" loginResp
        xsrfCookie <- sessionCookie "XSRF-TOKEN" loginResp
        let cookies = "JWT-Cookie=" <> cookieValuePart jwtCookie <> "; XSRF-TOKEN=" <> cookieValuePart xsrfCookie
        req <- localRequest port "/admin/users"
        let create name xsrfHeaders = httpLbs (req {HC.method = "POST", HC.requestBody = HC.RequestBodyLBS (encode $ object ["createUsername" .= (name :: Text), "createDisplayName" .= name, "createEmail" .= (name <> "@example.test"), "createMode" .= ("local" :: Text), "createPassword" .= ("password123" :: Text)]), HC.requestHeaders = [("Cookie", cookies), ("Content-Type", "application/json")] <> xsrfHeaders}) manager
        missing <- create "missing-xsrf" []
        assertEqual "missing XSRF" status401 (HC.responseStatus missing)
        wrong <- create "wrong-xsrf" [("X-XSRF-TOKEN", "incorrect-token")]
        assertEqual "wrong XSRF" status401 (HC.responseStatus wrong)
        valid <- create "cookie-created" [("X-XSRF-TOKEN", cookieValuePart xsrfCookie)]
        assertEqual "matching XSRF" status200 (HC.responseStatus valid)
        adminTok <- setupAdmin env "admin" "adminpass123"
        let admin = mkAdminTC adminTok
        users <- runClient env $ cAdminUsers admin Nothing Nothing Nothing (Just "username")
        assertEqual "only authorized mutation persisted" ["cookie-created"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems users)
        _ <- runClient env $ cAdminCreateUser admin (AdminCreateUserRequest "bearer-created" "bearer-created" ("bearer-created" <> "@example.test") AdminCreateLocal (Just "password123"))
        afterBearer <- runClient env $ cAdminUsers admin Nothing Nothing Nothing (Just "username")
        assertEqual "bearer admin needs no XSRF" ["bearer-created", "cookie-created"] (map (\summary -> userName $ profileUser summary.profile) $ pageItems afterBearer)
    ]
  where
    allowedOrigin = "http://127.0.0.1:3000"
    allowClient env = env {appClientOrigins = ["http://127.0.0.1:3000"]}
    preflight port manager path origin = do
      req <- localRequest port path
      httpLbs (req {HC.method = "OPTIONS", HC.requestHeaders = [("Origin", origin), ("Access-Control-Request-Method", "PUT"), ("Access-Control-Request-Headers", "Authorization, Content-Type")]}) manager
    assertAllowedOrigin resp = do
      assertEqual "exact allowed origin" (Just allowedOrigin) (lookup "Access-Control-Allow-Origin" $ HC.responseHeaders resp)
      assertEqual "no credentialed cookies" Nothing (lookup "Access-Control-Allow-Credentials" $ HC.responseHeaders resp)
      assertBool "origin-sensitive cache" ("origin" `elem` maybe [] headerTokens (lookup "Vary" $ HC.responseHeaders resp))
    assertNoOrigin resp = assertEqual "no cross-origin authorization" Nothing (lookup "Access-Control-Allow-Origin" $ HC.responseHeaders resp)
    checkCookieSecurity secure = withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") (\env -> env {appAdminCookieSecure = secure}) $ \_ port manager _ -> do
      loginResp <- loginAdminSession port manager
      jwtCookie <- sessionCookie "JWT-Cookie" loginResp
      xsrfCookie <- sessionCookie "XSRF-TOKEN" loginResp
      req <- localRequest port "/admin/auth/session/logout"
      logoutResp <- httpLbs (req {HC.method = "POST", HC.requestHeaders = [("Cookie", "JWT-Cookie=" <> cookieValuePart jwtCookie <> "; XSRF-TOKEN=" <> cookieValuePart xsrfCookie), ("X-XSRF-TOKEN", cookieValuePart xsrfCookie)]}) manager
      assertEqual "logout status" status200 (HC.responseStatus logoutResp)
      forM_ [("login", loginResp), ("logout", logoutResp)] $ \(phase, resp) ->
        forM_ ["JWT-Cookie", "XSRF-TOKEN"] $ \name -> do
          cookie <- sessionCookie name resp
          let attributes = headerTokensBySemicolon cookie
          assertEqual (phase <> " " <> B8.unpack name <> " Secure flag") secure ("secure" `elem` attributes)

-- User tests
userTests :: TestTree
userTests =
  testGroup
    "Users"
    [ testCase "get me" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        u <- runClient env $ cGetMe tc
        assertEqual "username" "alice" (userName (profileUser u)),
      --
      testCase "delete me" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        let tc = mkTC tokAlice
        _ <- runClient env $ cDeleteMe tc
        -- Token is now invalid (user gone), so /users/me should fail
        runClientExpectStatus env status401 $ cGetMe tc
    ]

-- Group tests
groupTests :: TestTree
groupTests =
  testGroup
    "Groups"
    [ testCase "create group" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        assertEqual "name" "Trip" (grpName g)
        assertEqual "owner" "alice" (grpOwnerName g)
        assertEqual "owner is auto-member" ["alice"] (grpMemberNames g),
      --
      testCase "add and deactivate member" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        let gid = grpId g
        g' <- runClient env $ cAddMember tc gid bobId
        assertEqual "has both" ["alice", "bob"] (grpMemberNames g')
        _ <- runClient env $ cDeleteMember tc gid bobId
        g'' <- runClient env $ cGetGroupById tc gid
        assertEqual "only owner after deactivation" ["alice"] (grpMemberNames g'')
        -- Re-adding reactivates
        g''' <- runClient env $ cAddMember tc gid bobId
        assertEqual "reactivated" ["alice", "bob"] (grpMemberNames g'''),
      --
      testCase "member can leave group" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let gid = grpId g
        _ <- runClient env $ cAddMember tcAlice gid bobId
        -- Bob leaves on his own
        _ <- runClient env $ cDeleteMember tcBob gid bobId
        g' <- runClient env $ cGetGroupById tcAlice gid
        assertEqual "bob left" ["alice"] (grpMemberNames g'),
      --
      testCase "cannot deactivate group owner" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        runClientExpectStatus env status400 $ cDeleteMember tc (grpId g) aliceId,
      --
      testCase "delete group cascades" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cDeleteGroupById tc (grpId g)
        runClientExpectStatus env status404 $ cGetGroupById tc (grpId g),
      --
      testCase "update group name" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Old Name"
        g' <- runClient env $ cUpdateGroupById tc (grpId g) "New Name"
        assertEqual "updated" "New Name" (grpName g'),
      --
      testCase "my groups endpoint" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        _ <- runClient env $ cCreateGroup tc "Trip1"
        _ <- runClient env $ cCreateGroup tc "Trip2"
        gs <- runClient env $ withPagination (cGetMyGroups tc) defaultPaginationParams
        assertEqual "two groups" 2 (length $ pageItems gs)
    ]

-- Record tests
recordTests :: TestTree
recordTests =
  testGroup
    "Records"
    [ recordNoteTests,
      recordQueryApiTests,
      testCase "create expense record with splits" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req =
              usdExpense
                ("Dinner")
                (100)
                (aliceId)
                (read "2025-06-01")
                ( [ RecordSplitRequest (aliceId) (MinorAmount (1)),
                    RecordSplitRequest (bobId) (MinorAmount (1))
                  ]
                )
        r <- runClient env $ cAddExpense tc (grpId g) req
        case r of
          ExpenseRecord {..} -> do
            assertEqual "title" "Dinner" title
            assertEqual "splits count" 2 (length splits)
          _ -> assertFailure "Expected ExpenseRecord",
      --
      testCase "create transfer record" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req =
              usdTransfer (50) (aliceId) (bobId) (read "2025-06-01")
        r <- runClient env $ cAddTransfer tc (grpId g) req
        case r of
          TransferRecord {..} -> do
            assertEqual "from" "alice" (userName paidBy)
            assertEqual "to" "bob" (userName transferTo)
          _ -> assertFailure "Expected TransferRecord",
      --
      testCase "delete record" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req =
              usdExpense ("Lunch") (30) (aliceId) (read "2025-06-01") ([RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))])
        r <- runClient env $ cAddExpense tc (grpId g) req
        _ <- runClient env $ cDeleteRecord tc (grpId g) (extractRecordId r)
        runClientExpectStatus env status404 $ cGetRecord tc (grpId g) (extractRecordId r),
      --
      testCase "list records in group" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req1 =
              usdExpense ("Lunch") (30) (aliceId) (read "2025-06-01") ([RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))])
            req2 =
              usdTransfer (10) (bobId) (aliceId) (read "2025-06-02")
        _ <- runClient env $ cAddExpense tc (grpId g) req1
        _ <- runClient env $ cAddTransfer tc (grpId g) req2
        rs <- runClient env $ withPagination1 (cGetRecords tc) (grpId g) defaultPaginationParams
        assertEqual "two records" 2 (length $ pageItems rs),
      --
      testCase "transfer to self => 400" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        let req =
              usdTransfer (10) (aliceId) (aliceId) (read "2025-06-01")
        runClientExpectStatus env status400 $ cAddTransfer tc (grpId g) req,
      --
      testCase "zero amount => 400" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        let req =
              usdExpense ("Free") (0) (aliceId) (read "2025-06-01") ([RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))])
        runClientExpectStatus env status400 $ cAddExpense tc (grpId g) req
    ]

sharedIncomeTests :: TestTree
sharedIncomeTests =
  testGroup
    "Shared income"
    [ testCase "user and admin CRUD recompute balances and retain historical identities" $ fixture $ \env tc admin aliceId bobId _ -> do
        group <- runClient env $ cCreateGroup tc "Income lifecycle"
        let gid = grpId group
            request = income bobId 6000 [aliceId, bobId]
        _ <- runClient env $ cAddMember tc gid bobId
        created <- runClient env $ cAddIncome tc gid request
        assertIncome bobId 6000 created
        fetched <- runClient env $ cGetRecord tc gid (extractRecordId created)
        assertEqual "GET retains income fields and allocations" (canonical created) (canonical fetched)
        listed <- runClient env $ withPagination1 (cGetRecords tc) gid defaultPaginationParams
        assertEqual "income is in generic list" [canonical created] (map canonical $ pageItems listed)
        adminListed <- runClient env $ allAdminRecords admin gid
        assertEqual "income is in admin list" [canonical created] (map canonical adminListed)
        assertBalances env tc gid [("alice", 3000), ("bob", -3000)]
        case created of
          IncomeRecord {splits = shares} -> assertEqual "beneficiary entitlements" [("alice", MinorAmount 3000), ("bob", MinorAmount 3000)] (sort [(userName person, amount) | RecordSplit person _ _ amount <- shares])
          _ -> assertFailure "Expected income"
        edited <- runClient env $ cAdminUpdateIncome admin gid (extractRecordId created) (income bobId 10000 [aliceId, bobId])
        assertIncome bobId 10000 edited
        assertEqual "editing keeps record ID" (extractRecordId created) (extractRecordId edited)
        case (created, edited) of
          (IncomeRecord {createdAt = originalTime}, IncomeRecord {createdAt = editedTime}) -> assertEqual "editing keeps creation time" originalTime editedTime
          _ -> assertFailure "Expected income records"
        assertBalances env tc gid [("alice", 5000), ("bob", -5000)]
        _ <- runClient env $ cDeleteMember tc gid bobId
        historical <- runClient env $ cGetRecord tc gid (extractRecordId created)
        assertEqual "former recipient and beneficiary retained" (canonical edited) (canonical historical)
        assertBalances env tc gid [("alice", 5000), ("bob", -5000)]
        _ <- runClient env $ cDeleteRecord tc gid (extractRecordId created)
        runClientExpectStatus env status404 $ cGetRecord tc gid (extractRecordId created)
        assertBalances env tc gid [("alice", 0), ("bob", 0)]
        _ <- runClient env $ cAddMember tc gid bobId
        adminCreated <- runClient env $ cAdminAddIncome admin gid (income aliceId 7001 [bobId])
        assertIncome aliceId 7001 adminCreated
        assertBalances env tc gid [("alice", -7001), ("bob", 7001)]
        userEdited <- runClient env $ cUpdateIncome tc gid (extractRecordId adminCreated) (income bobId 9000 [aliceId])
        assertIncome bobId 9000 userEdited
        assertBalances env tc gid [("alice", 9000), ("bob", -9000)]
        _ <- runClient env $ cAdminDeleteRecord admin gid (extractRecordId userEdited)
        runClientExpectStatus env status404 $ cGetRecord tc gid (extractRecordId userEdited)
        assertBalances env tc gid [("alice", 0), ("bob", 0)],
      testCase "wrong-kind updates reject without changing records, splits or report" $ fixture $ \env tc admin aliceId bobId _ -> do
        group <- runClient env $ cCreateGroup tc "Kind preservation"
        let gid = grpId group
            incomeRequest = income bobId 6000 [aliceId, bobId]
            expenseRequest = usdExpense "Expense" 80 aliceId day [RecordSplitRequest aliceId (MinorAmount 1), RecordSplitRequest bobId (MinorAmount 1)]
            transferRequest = usdTransfer 20 aliceId bobId day
        _ <- runClient env $ cAddMember tc gid bobId
        expense <- runClient env $ cAddExpense tc gid expenseRequest
        transfer <- runClient env $ cAddTransfer tc gid transferRequest
        received <- runClient env $ cAddIncome tc gid incomeRequest
        report <- runClient env $ cGetReport tc gid
        let assertUnchanged = do
              forM_ [expense, transfer, received] $ \original -> do
                saved <- runClient env $ cGetRecord tc gid (extractRecordId original)
                assertEqual "wrong-kind update preserves every field" (canonical original) (canonical saved)
              savedReport <- runClient env $ cGetReport tc gid
              assertEqual "wrong-kind update preserves balances" report savedReport
            expectNotIncome action = do
              result <- runClientM action env
              case result of
                Left (FailureResponse _ response) -> do
                  assertEqual "wrong income kind status" status400 (responseStatusCode response)
                  assertEqual "wrong income kind message" "Record is not an income record" (responseBody response)
                other -> assertFailure $ "Expected wrong-kind rejection, got " <> show other
              assertUnchanged
        forM_ [expense, transfer] $ \record -> do
          expectNotIncome $ cUpdateIncome tc gid (extractRecordId record) incomeRequest
          expectNotIncome $ cAdminUpdateIncome admin gid (extractRecordId record) incomeRequest
        forM_ [cUpdateExpense tc gid (extractRecordId received) expenseRequest, cAdminUpdateExpense admin gid (extractRecordId received) expenseRequest, cUpdateTransfer tc gid (extractRecordId received) transferRequest, cAdminUpdateTransfer admin gid (extractRecordId received) transferRequest] $ \action -> do
          runClientExpectStatus env status400 action
          assertUnchanged,
      testCase "invalid, nonmember and inactive participants fail atomically" $ fixture $ \env tc admin aliceId bobId outsiderId -> do
        group <- runClient env $ cCreateGroup tc "Income validation"
        let gid = grpId group
            absentId = UserId UUID.nil
            valid :: IncomeRecordRequest
            valid = income aliceId 6000 [aliceId]
            invalid =
              [ (status400, income outsiderId 6000 [aliceId]),
                (status400, income aliceId 6000 [outsiderId]),
                (status400, income bobId 6000 [aliceId]),
                (status400, income aliceId 6000 [bobId]),
                (status404, income absentId 6000 [aliceId]),
                (status404, income aliceId 6000 [absentId]),
                (status400, income aliceId 0 [aliceId]),
                (status400, income aliceId (-1) [aliceId]),
                (status400, income aliceId 6000 []),
                (status400, income aliceId 6000 [aliceId, aliceId]),
                (status400, IncomeRecordRequest "" Nothing (MinorAmount 6000) (CurrencyCode "USD") aliceId day EqualSplit [RecordSplitRequest aliceId (MinorAmount 1)] Nothing),
                (status400, IncomeRecordRequest "Invalid split" Nothing (MinorAmount 6000) (CurrencyCode "USD") aliceId day EqualSplit [RecordSplitRequest aliceId (MinorAmount (-1))] Nothing),
                (status400, IncomeRecordRequest "Invalid exact" Nothing (MinorAmount 6000) (CurrencyCode "USD") aliceId day ExactSplit [RecordSplitRequest aliceId (MinorAmount 5999)] Nothing)
              ]
        _ <- runClient env $ cAddMember tc gid bobId
        _ <- runClient env $ cDeleteMember tc gid bobId
        forM_ invalid $ \(status, request) -> do
          runClientExpectStatus env status $ cAddIncome tc gid request
          runClientExpectStatus env status $ cAdminAddIncome admin gid request
          records <- runClient env $ allAdminRecords admin gid
          assertEqual "failed create saves no records" [] records
          unchanged <- runClient env $ cGetGroupById tc gid
          assertEqual "failed first income leaves policy unlocked" False unchanged.currencyLocked
        original <- runClient env $ cAddIncome tc gid valid
        report <- runClient env $ cGetReport tc gid
        forM_ invalid $ \(status, request) -> do
          runClientExpectStatus env status $ cUpdateIncome tc gid (extractRecordId original) request
          runClientExpectStatus env status $ cAdminUpdateIncome admin gid (extractRecordId original) request
          saved <- runClient env $ cGetRecord tc gid (extractRecordId original)
          assertEqual "invalid update preserves fields and allocations" (canonical original) (canonical saved)
          savedReport <- runClient env $ cGetReport tc gid
          assertEqual "invalid update preserves report" report savedReport,
      testCase "authorization, missing records and cross-group updates preserve income" $ fixture $ \env tc admin aliceId bobId _ -> do
        group <- runClient env $ cCreateGroup tc "Income access"
        otherGroup <- runClient env $ cCreateGroup tc "Other group"
        bobToken <- loginWithPassword env "bob" "password123"
        let gid = grpId group
            otherGid = grpId otherGroup
            bob = mkTC bobToken
            request = income aliceId 6000 [aliceId]
            missing = RecordId UUID.nil
        original <- runClient env $ cAddIncome tc gid request
        let rid = extractRecordId original
        runClientExpectStatus env status403 $ cAddIncome bob gid request
        runClientExpectStatus env status403 $ cUpdateIncome bob gid rid request
        runClientExpectStatus env status403 $ cGetRecord bob gid rid
        runClientExpectStatus env status403 $ cDeleteRecord bob gid rid
        runClientExpectStatus env status401 $ cAdminAddIncome (mkAdminTC bobToken) gid request
        runClientExpectStatus env status401 $ cAdminUpdateIncome (mkAdminTC bobToken) gid rid request
        forM_ [cUpdateIncome tc otherGid rid request, cAdminUpdateIncome admin otherGid rid request, cUpdateIncome tc gid missing request, cAdminUpdateIncome admin gid missing request] $ runClientExpectStatus env status404
        saved <- runClient env $ cGetRecord tc gid rid
        assertEqual "rejected access preserves income" (canonical original) (canonical saved)
        otherRecords <- runClient env $ allAdminRecords admin otherGid
        assertEqual "cross-group update does not insert" [] otherRecords
        _ <- runClient env $ cAddMember tc gid bobId
        updated <- runClient env $ cUpdateIncome bob gid rid (income bobId 8000 [aliceId])
        assertIncome bobId 8000 updated,
      testCase "first user or admin income locks currency permanently" $ fixture $ \env tc admin aliceId _ _ -> do
        token <- loginWithPassword env "alice" "password123"
        let (_ :<|> _ :<|> updatePolicy :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _) = client (Proxy :: Proxy (Auth '[JWT] AuthUser :> GroupAPI)) (Token $ encodeUtf8 token)
        forM_ [("User income", cAddIncome tc), ("Admin income", cAdminAddIncome admin)] $ \(name, create) -> do
          group <- runClient env $ cCreateGroup tc name
          let gid = grpId group
          assertEqual "new group starts unlocked" False group.currencyLocked
          received <- runClient env $ create gid (income aliceId 1 [aliceId])
          locked <- runClient env $ cGetGroupById tc gid
          assertEqual "first income locks policy" True locked.currencyLocked
          runClientExpectStatus env status409 $ updatePolicy gid (UpdateGroupRequest "Must not rename" MultiCurrency (CurrencyCode "EUR"))
          unchanged <- runClient env $ cGetGroupById tc gid
          assertEqual "policy conflict changes no fields" locked unchanged
          _ <- runClient env $ cDeleteRecord tc gid (extractRecordId received)
          afterDelete <- runClient env $ cGetGroupById tc gid
          assertEqual "deleting final income retains currency lock" True afterDelete.currencyLocked
    ]
  where
    day = read "2026-10-14"
    income :: UserId -> Integer -> [UserId] -> IncomeRecordRequest
    income actor amount beneficiaries = IncomeRecordRequest "Shared hosting income" Nothing (MinorAmount amount) (CurrencyCode "USD") actor day EqualSplit [RecordSplitRequest person (MinorAmount 1) | person <- beneficiaries] Nothing
    canonical :: Record -> Record
    canonical record@IncomeRecord {splits = shares} = record {StoredRecord.splits = sort shares}
    canonical record@ExpenseRecord {splits = shares} = record {StoredRecord.splits = sort shares}
    canonical record = record
    assertIncome actor amount record = case record of
      IncomeRecord {receivedBy = receiver, amountMinor = original, settlementAmountMinor = settled} -> do
        assertEqual "cash recipient" actor (userIdOf receiver)
        assertEqual "positive original magnitude" (MinorAmount amount) original
        assertEqual "settlement magnitude" (MinorAmount amount) settled
      _ -> assertFailure "Expected income record"
    assertBalances env tc gid expected = do
      report <- runClient env $ cGetReport tc gid
      assertEqual "income balance effects" (sort [(name, MinorAmount amount) | (name, amount) <- expected]) (sort [(userName person, total) | Balance {user = person, totalMinor = total} <- concatMap balances (Report.currencies report)])
    fixture action = withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
      token <- setupUser env "alice"
      aliceId <- fixtureUserId env "alice"
      _ <- setupUser env "bob"
      bobId <- fixtureUserId env "bob"
      _ <- setupUser env "outsider"
      outsiderId <- fixtureUserId env "outsider"
      adminToken <- setupAdmin env "admin" "adminpass123"
      action env (mkTC token) (mkAdminTC adminToken) aliceId bobId outsiderId

-- Report / settlement integration test
reportIntegrationTests :: TestTree
reportIntegrationTests =
  testGroup
    "Report"
    [ testCase "settlement of simple expenses" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        -- Alice pays $100, split evenly
        let req =
              usdExpense
                ("Hotel")
                (100)
                (aliceId)
                (read "2025-06-01")
                ( [ RecordSplitRequest (aliceId) (MinorAmount (1)),
                    RecordSplitRequest (bobId) (MinorAmount (1))
                  ]
                )
        _ <- runClient env $ cAddExpense tc (grpId g) req
        report <- runClient env $ cGetReport tc (grpId g)
        -- Bob owes Alice $50
        assertEqual "one settlement" 1 (length $ concatMap settlements (Report.currencies report))
        case concatMap settlements (Report.currencies report) of
          [Settlement sFrom sTo sAmt] -> do
            assertEqual "from" "bob" (userName sFrom)
            assertEqual "to" "alice" (userName sTo)
            assertMoneyMajor ("amount") (50) (sAmt)
          _ -> assertFailure "Expected exactly one settlement"
    ]

-- Authorization tests
authorizationTests :: TestTree
authorizationTests =
  testGroup
    "Authorization"
    [ testCase "non-member cannot view group => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cGetGroupById tcBob (grpId g),
      --
      testCase "non-owner cannot add member => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        _ <- setupUser env "charlie"
        charlieId <- fixtureUserId env "charlie"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        -- Bob (not owner) tries to add charlie
        runClientExpectStatus env status403 $ cAddMember tcBob (grpId g) charlieId,
      --
      testCase "non-owner cannot update group => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cUpdateGroupById tcBob (grpId g) "Hacked",
      --
      testCase "non-owner cannot delete group => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cDeleteGroupById tcBob (grpId g),
      --
      testCase "non-member cannot add records => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let req = usdTransfer (10) (aliceId) (bobId) ((read "2025-06-01"))
        runClientExpectStatus env status403 $ cAddTransfer tcBob (grpId g) req,
      --
      testCase "non-member cannot view records => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ withPagination1 (cGetRecords tcBob) (grpId g) defaultPaginationParams,
      --
      testCase "non-member cannot view report => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cGetReport tcBob (grpId g),
      --
      testCase "member can add records" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        let req = usdTransfer (10) (aliceId) (bobId) ((read "2025-06-01"))
        r <- runClient env $ cAddTransfer tcBob (grpId g) req
        case r of
          TransferRecord {} -> pure ()
          _ -> assertFailure "Expected TransferRecord",
      --
      testCase "adding duplicate member => 409" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        runClientExpectStatus env status409 $ cAddMember tcAlice (grpId g) bobId,
      --
      testCase "transfer ownership" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        -- Transfer ownership to bob
        g' <- runClient env $ cTransferOwnership tcAlice (grpId g) bobId
        assertEqual "new owner" "bob" (grpOwnerName g'),
      --
      testCase "transfer ownership to non-member => 400" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status400 $ cTransferOwnership tcAlice (grpId g) bobId,
      --
      testCase "non-owner cannot transfer ownership => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        runClientExpectStatus env status403 $ cTransferOwnership tcBob (grpId g) bobId,
      --
      testCase "new owner can manage group after transfer" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        _ <- setupUser env "charlie"
        charlieId <- fixtureUserId env "charlie"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) bobId
        -- Transfer to bob
        _ <- runClient env $ cTransferOwnership tcAlice (grpId g) bobId
        -- Bob can now add members
        g' <- runClient env $ cAddMember tcBob (grpId g) charlieId
        assertEqual "three members" ["alice", "bob", "charlie"] (grpMemberNames g')
        -- Alice (former owner) can no longer add members
        runClientExpectStatus env status403 $ cAddMember tcAlice (grpId g) aliceId
    ]

-- Receipt tests
receiptTests :: TestTree
receiptTests =
  testGroup
    "Receipts"
    [ testCase "create receipt with multiple expense records" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req =
              CreateReceiptRequest
                { note = "Restaurant bill",
                  records =
                    [ usdExpense
                        ("Dinner")
                        (100)
                        (aliceId)
                        (read "2025-06-01")
                        ( [ RecordSplitRequest (aliceId) (MinorAmount (1)),
                            RecordSplitRequest (bobId) (MinorAmount (1))
                          ]
                        ),
                      usdExpense
                        ("Drinks")
                        (40)
                        (bobId)
                        (read "2025-06-01")
                        ( [ RecordSplitRequest (aliceId) (MinorAmount (1)),
                            RecordSplitRequest (bobId) (MinorAmount (1))
                          ]
                        )
                    ]
                }
        r <- runClient env $ cCreateReceipt tc (grpId g) req
        assertEqual "note" "Restaurant bill" (receiptNote r)
        assertEqual "records count" 2 (length $ receiptRecords r)
        assertEqual "uploaded by" "alice" (userName $ uploadedBy r),
      --
      testCase "list receipts in group" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req1 =
              CreateReceiptRequest
                { note = "Receipt 1",
                  records =
                    [ usdExpense "Lunch" 50 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
                    ]
                }
            req2 =
              CreateReceiptRequest
                { note = "Receipt 2",
                  records =
                    [ usdExpense "Snacks" 15 bobId (read "2025-06-02") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
                    ]
                }
        _ <- runClient env $ cCreateReceipt tc (grpId g) req1
        _ <- runClient env $ cCreateReceipt tc (grpId g) req2
        rs <- runClient env $ withPagination1 (cGetReceipts tc) (grpId g) defaultPaginationParams
        assertEqual "two receipts" 2 (length $ pageItems rs),
      --
      testCase "get receipt by id" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req =
              CreateReceiptRequest
                { note = "Solo receipt",
                  records =
                    [ usdExpense "Coffee" 5 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
                    ]
                }
        created <- runClient env $ cCreateReceipt tc (grpId g) req
        fetched <- runClient env $ cGetReceipt tc (grpId g) (receiptId created)
        assertEqual "note matches" "Solo receipt" (receiptNote fetched)
        assertEqual "records count" 1 (length $ receiptRecords fetched),
      --
      testCase "delete receipt deletes associated records" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        let req =
              CreateReceiptRequest
                { note = "To delete",
                  records =
                    [ usdExpense "Item" 10 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
                    ]
                }
        created <- runClient env $ cCreateReceipt tc (grpId g) req
        case receiptRecords created of
          [record] -> do
            let rctId = receiptId created
                recId = extractRecordId record
            _ <- runClient env $ cDeleteReceipt tc (grpId g) rctId
            -- Receipt should be gone
            runClientExpectStatus env status404 $ cGetReceipt tc (grpId g) rctId
            -- The record should also be gone
            runClientExpectStatus env status404 $ cGetRecord tc (grpId g) recId
          _ -> assertFailure "Expected exactly one receipt record",
      --
      testCase "non-member cannot create receipt => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let req =
              CreateReceiptRequest
                { note = "Unauthorized",
                  records =
                    [ usdExpense "Unauthorized item" 10 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
                    ]
                }
        runClientExpectStatus env status403 $ cCreateReceipt tcBob (grpId g) req,
      --
      testCase "receipt records appear in group records listing" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) bobId
        -- Create a receipt with one record
        let req =
              CreateReceiptRequest
                { note = "Via receipt",
                  records =
                    [ usdExpense "Taxi" 25 aliceId (read "2025-06-01") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
                    ]
                }
        _ <- runClient env $ cCreateReceipt tc (grpId g) req
        -- The record should also appear in the group's record listing
        rs <- runClient env $ withPagination1 (cGetRecords tc) (grpId g) defaultPaginationParams
        assertEqual "one record in group" 1 (length $ pageItems rs)
    ]

integrityTests :: TestTree
integrityTests =
  testGroup
    "Integrity"
    [ testCase "foreign record and receipt IDs are 404 and mixed bulk deletes change neither group" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        tokBob <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        tokCharlie <- setupUser env "charlie"
        charlieId <- fixtureUserId env "charlie"
        let admin = mkAdminTC tokAdmin
            alice = mkTC tokAlice
            bob = mkTC tokBob
            charlie = mkTC tokCharlie
            day = read "2026-10-14"
            expenseFor payer = usdExpense "Original dinner" 90 payer day [RecordSplitRequest (payer) (MinorAmount (1)), RecordSplitRequest (charlieId) (MinorAmount (2))]
            transferFor payer = usdTransfer (15) (payer) (charlieId) (day)
            changedExpense = usdExpense "Should not be saved" 42 aliceId (read "2026-10-15") [RecordSplitRequest (aliceId) (MinorAmount (1))]
            changedTransfer = usdTransfer (42) (charlieId) (aliceId) ((read "2026-10-15"))
            changedReceipt = UpdateReceiptRequest "Should not replace children" [changedExpense]
        groupA <- runClient env $ cCreateGroup alice "A"
        groupB <- runClient env $ cCreateGroup bob "B"
        let gidA = grpId groupA
            gidB = grpId groupB
        _ <- runClient env $ cAddMember alice gidA charlieId
        _ <- runClient env $ cAddMember bob gidB charlieId
        localExpense <- runClient env $ cAddExpense alice gidA (expenseFor aliceId)
        localTransfer <- runClient env $ cAddTransfer alice gidA (transferFor aliceId)
        localReceipt <- runClient env $ cCreateReceipt alice gidA (CreateReceiptRequest "A batch" [expenseFor aliceId])
        foreignExpense <- runClient env $ cAddExpense bob gidB (expenseFor bobId)
        foreignTransfer <- runClient env $ cAddTransfer bob gidB (transferFor bobId)
        foreignReceipt <- runClient env $ cCreateReceipt bob gidB (CreateReceiptRequest "B batch" [expenseFor bobId])
        let foreignIds = map extractRecordId [foreignExpense, foreignTransfer] ++ map extractRecordId (receiptRecords foreignReceipt)
            foreignReceiptId = receiptId foreignReceipt
            normalizeRecord :: Record -> Record
            normalizeRecord record@ExpenseRecord {splits = values} = record {StoredRecord.splits = sort values}
            normalizeRecord record = record
            normalizeReceipt (Receipt rid gid uploader batchNote batchRecords created) =
              Receipt rid gid uploader batchNote (sort $ map normalizeRecord batchRecords) created
            assertRecordUnchanged tc gid expected = do
              actual <- runClient env $ cGetRecord tc gid (extractRecordId expected)
              assertEqual "record fields and splits unchanged" (normalizeRecord expected) (normalizeRecord actual)
            assertReceiptUnchanged tc gid expected = do
              actual <- runClient env $ cGetReceipt tc gid (receiptId expected)
              assertEqual "receipt fields, child IDs and splits unchanged" (normalizeReceipt expected) (normalizeReceipt actual)
              mapM_ (assertRecordUnchanged tc gid) (receiptRecords expected)
            assertBothGroupsUnchanged = do
              mapM_ (assertRecordUnchanged alice gidA) [localExpense, localTransfer]
              assertReceiptUnchanged alice gidA localReceipt
              mapM_ (assertRecordUnchanged bob gidB) [foreignExpense, foreignTransfer]
              assertReceiptUnchanged bob gidB foreignReceipt
            checkUserRecord rid = do
              runClientExpectStatus env status404 $ cGetRecord alice gidA rid
              runClientExpectStatus env status404 $ cDeleteRecord alice gidA rid
              runClientExpectStatus env status404 $ cUpdateExpense alice gidA rid changedExpense
              runClientExpectStatus env status404 $ cUpdateTransfer alice gidA rid changedTransfer
            checkAdminRecord rid = do
              runClientExpectStatus env status404 $ cAdminDeleteRecord admin gidA rid
              runClientExpectStatus env status404 $ cAdminUpdateExpense admin gidA rid changedExpense
              runClientExpectStatus env status404 $ cAdminUpdateTransfer admin gidA rid changedTransfer
        mapM_ checkUserRecord foreignIds
        runClientExpectStatus env status404 $ cGetReceipt alice gidA foreignReceiptId
        runClientExpectStatus env status404 $ cDeleteReceipt alice gidA foreignReceiptId
        runClientExpectStatus env status404 $ cUpdateReceipt alice gidA foreignReceiptId changedReceipt
        assertBothGroupsUnchanged
        mapM_ checkAdminRecord foreignIds
        runClientExpectStatus env status404 $ cAdminDeleteReceipt admin gidA foreignReceiptId
        runClientExpectStatus env status404 $ cAdminUpdateReceipt admin gidA foreignReceiptId changedReceipt
        assertBothGroupsUnchanged
        -- Put the valid ID first so sequential validate-and-delete would lose it.
        runClientExpectStatus env status404 $ cAdminBulkDeleteRecords admin gidA (BulkRequest [extractRecordId localExpense, extractRecordId foreignExpense])
        assertBothGroupsUnchanged
        runClientExpectStatus env status404 $ cAdminBulkDeleteRecords admin gidA (BulkRequest [extractRecordId localTransfer, extractRecordId foreignTransfer])
        assertBothGroupsUnchanged
        runClientExpectStatus env status404 $ cAdminBulkDeleteReceipts admin gidA (BulkRequest [receiptId localReceipt, foreignReceiptId])
        assertBothGroupsUnchanged
        -- An unauthorized URL group still fails authorization, not resource lookup.
        runClientExpectStatus env status403 $ cGetRecord alice gidB (extractRecordId foreignExpense)
        runClientExpectStatus env status403 $ cGetReceipt alice gidB foreignReceiptId
        -- Ordinary same-group members retain read and mutation access.
        assertRecordUnchanged charlie gidA localExpense
        assertReceiptUnchanged charlie gidA localReceipt
        updatedExpense <- runClient env $ cUpdateExpense charlie gidA (extractRecordId localExpense) changedExpense
        assertEqual "same-group expense update" "Should not be saved" (titleOfRecord updatedExpense)
        updatedTransfer <- runClient env $ cUpdateTransfer charlie gidA (extractRecordId localTransfer) changedTransfer
        assertRecordUnchanged alice gidA updatedTransfer
        updatedReceipt <- runClient env $ cUpdateReceipt charlie gidA (receiptId localReceipt) changedReceipt
        assertReceiptUnchanged alice gidA updatedReceipt
        _ <- runClient env $ cDeleteRecord charlie gidA (extractRecordId localExpense)
        _ <- runClient env $ cDeleteRecord charlie gidA (extractRecordId localTransfer)
        _ <- runClient env $ cDeleteReceipt charlie gidA (receiptId localReceipt)
        runClientExpectStatus env status404 $ cGetRecord alice gidA (extractRecordId localExpense)
        runClientExpectStatus env status404 $ cGetRecord alice gidA (extractRecordId localTransfer)
        runClientExpectStatus env status404 $ cGetReceipt alice gidA (receiptId localReceipt),
      testCase "admin receipt uploader must be an active group member before any write" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "outsider"
        outsiderId <- fixtureUserId env "outsider"
        _ <- setupUser env "former"
        formerId <- fixtureUserId env "former"
        _ <- setupUser env "member"
        memberId <- fixtureUserId env "member"
        let admin = mkAdminTC tokAdmin
            alice = mkTC tokAlice
            child = usdExpense "Must not be inserted" 30 aliceId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (1))]
        group <- runClient env $ cCreateGroup alice "Uploader validation"
        let gid = grpId group
            assertNoWrites = do
              receipts <- runClient env $ cAdminGroupReceipts admin gid
              records <- runClient env $ allAdminRecords admin gid
              assertEqual "no receipt inserted" [] receipts
              assertEqual "no child expense inserted" [] records
        _ <- runClient env $ cAddMember alice gid formerId
        _ <- runClient env $ cDeleteMember alice gid formerId
        _ <- runClient env $ cAddMember alice gid memberId
        runClientExpectStatus env status400 $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest outsiderId "Rejected" [child])
        assertNoWrites
        runClientExpectStatus env status404 $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest (UserId UUID.nil) "Rejected" [child])
        assertNoWrites
        runClientExpectStatus env status400 $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest formerId "Rejected" [child])
        assertNoWrites
        accepted <- runClient env $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest memberId "Accepted" [child])
        assertEqual "active member uploader" "member" (userName $ uploadedBy accepted)
        assertEqual "accepted child title" ["Must not be inserted"] (map titleOfRecord $ receiptRecords accepted)
        fetched <- runClient env $ cGetReceipt alice gid (receiptId accepted)
        assertEqual "saved uploader" "member" (userName $ uploadedBy fetched)
        empty <- runClient env $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest aliceId "" [])
        assertEqual "empty note accepted" "" (receiptNote empty)
        assertEqual "empty expense batch accepted" [] (receiptRecords empty)
        savedEmpty <- runClient env $ cGetReceipt alice gid (receiptId empty)
        assertEqual "empty batch persists" empty savedEmpty,
      testCase "invalid exact requests and obsolete numeric fields reject without writes" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        adminTok <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC tok
            admin = mkAdminTC adminTok
            day = read "2026-10-14"
            expense value shares = usdExpense "Dinner" value aliceId day shares
            invalidExpenses :: [ExpenseRecordRequest]
            invalidExpenses =
              [ expense 100 [],
                expense 100 [RecordSplitRequest (aliceId) (MinorAmount (0)), RecordSplitRequest (bobId) (MinorAmount (0))],
                expense 100 [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (aliceId) (MinorAmount (1))],
                expense 100 [RecordSplitRequest (aliceId) (MinorAmount ((-1)))],
                expense 0 [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
              ]
        g <- runClient env $ cCreateGroup tc "Input boundaries"
        let gid@(GroupId uuid) = grpId g
            assertEmpty = do
              rs <- runClient env $ withPagination1 (cGetRecords tc) gid defaultPaginationParams
              assertEqual "invalid request leaves record list empty" [] (pageItems rs)
            expectInvalid action = do
              result <- runClientM action env
              case result of
                Left (FailureResponse _ response) -> do
                  assertEqual "invalid input status" status400 (responseStatusCode response)
                Left err -> assertFailure $ "Unexpected validation client error: " <> show err
                Right _ -> assertFailure "Expected invalid input to fail"
            rawReject token prefix variant body = do
              req0 <- parseRequest $ "http://localhost:" <> show port <> prefix <> show uuid <> "/records/" <> variant
              let req = req0 {HC.method = "POST", HC.requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> encodeUtf8 token)], HC.requestBody = HC.RequestBodyLBS body}
              resp <- httpLbs req manager
              assertEqual "obsolete wire field status" status400 (HC.responseStatus resp)
              assertEmpty
        _ <- runClient env $ cAddMember tc gid bobId
        mapM_
          ( \bad -> do
              expectInvalid $ cAddExpense tc gid bad
              expectInvalid $ cAdminAddExpense admin gid bad
              assertEmpty
          )
          invalidExpenses
        mapM_
          ( \amount' -> do
              let bad = usdTransfer (amount') (aliceId) (bobId) (day)
              runClientExpectStatus env status400 $ cAddTransfer tc gid bad
              runClientExpectStatus env status400 $ cAdminAddTransfer admin gid bad
              assertEmpty
          )
          [0, -1]
        mapM_
          ( \(token, prefix) -> do
              rawReject token prefix "expense" ("{\"title\":\"Obsolete\",\"amountMinor\":\"10000\",\"currency\":\"USD\",\"amount\":100,\"byUserId\":" <> encode aliceId <> ",\"date\":\"2026-10-14\",\"splitMode\":\"shares\",\"splits\":[{\"userId\":" <> encode aliceId <> ",\"value\":\"1\"}]}")
              rawReject token prefix "transfer" ("{\"amountMinor\":\"1000\",\"currency\":\"USD\",\"amount\":10,\"byUserId\":" <> encode aliceId <> ",\"toUserId\":" <> encode bobId <> ",\"date\":\"2026-10-14\"}")
          )
          [(tok, "/groups/"), (adminTok, "/admin/groups/")],
      testCase "shared expense validation protects receipt batches and user/admin updates" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        adminTok <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC tok
            admin = mkAdminTC adminTok
            valid = usdExpense "Original" 100 aliceId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]
            badRequests :: [ExpenseRecordRequest]
            badRequests =
              [ usdExpense "Original" 100 aliceId (read "2026-10-14") shares
              | shares <- [[], [RecordSplitRequest (aliceId) (MinorAmount (0))], [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (aliceId) (MinorAmount (1))], [RecordSplitRequest (aliceId) (MinorAmount ((-1)))]]
              ]
                ++ [usdExpense "Original" 0 aliceId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (1))]]
        g <- runClient env $ cCreateGroup tc "Shared validation"
        let gid = grpId g
        _ <- runClient env $ cAddMember tc gid bobId
        mapM_
          ( \bad -> do
              runClientExpectStatus env status400 $ cCreateReceipt tc gid (CreateReceiptRequest "Rejected" [valid, bad])
              runClientExpectStatus env status400 $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest aliceId "Rejected" [valid, bad])
              rs <- runClient env $ allAdminRecords admin gid
              receipts <- runClient env $ cAdminGroupReceipts admin gid
              assertEqual "invalid later child inserts no expense" [] rs
              assertEqual "invalid later child inserts no receipt" [] receipts
          )
          badRequests
        original <- runClient env $ cAddExpense tc gid valid
        receipt <- runClient env $ cCreateReceipt tc gid (CreateReceiptRequest "Original batch" [valid])
        transfer <- runClient env $ cAddTransfer tc gid (usdTransfer (10) (aliceId) (bobId) ((read "2026-10-14")))
        before <- runClient env $ allAdminRecords admin gid
        mapM_
          ( \bad -> do
              runClientExpectStatus env status400 $ cUpdateExpense tc gid (extractRecordId original) bad
              runClientExpectStatus env status400 $ cAdminUpdateExpense admin gid (extractRecordId original) bad
              runClientExpectStatus env status400 $ cUpdateReceipt tc gid (receiptId receipt) (UpdateReceiptRequest "Changed" [valid, bad])
              runClientExpectStatus env status400 $ cAdminUpdateReceipt admin gid (receiptId receipt) (UpdateReceiptRequest "Changed" [valid, bad])
              saved <- runClient env $ cGetReceipt tc gid (receiptId receipt)
              assertEqual "invalid replacement preserves note, IDs, and splits" receipt saved
              recordsAfter <- runClient env $ allAdminRecords admin gid
              assertEqual "invalid updates preserve every record" (sort before) (sort recordsAfter)
          )
          badRequests
        let zeroTransfer = usdTransfer (0) (aliceId) (bobId) ((read "2026-10-14"))
        runClientExpectStatus env status400 $ cUpdateTransfer tc gid (extractRecordId transfer) zeroTransfer
        runClientExpectStatus env status400 $ cAdminUpdateTransfer admin gid (extractRecordId transfer) zeroTransfer
        savedTransfer <- runClient env $ cGetRecord tc gid (extractRecordId transfer)
        assertEqual "invalid transfer update preserves original" transfer savedTransfer,
      testCase "wide shares and near-Int64 amounts preserve exact allocations" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Exact arithmetic"
        let gid = grpId g
            expense amount' a b = usdExpense "Weighted" amount' aliceId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (a)), RecordSplitRequest (bobId) (MinorAmount (b))]
            assertSplits expected record = case record of
              ExpenseRecord {splits = ss} -> do
                assertEqual "participants and weights" (sort [(n, w) | (n, w, _) <- expected]) (sort [(userName u, w) | RecordSplit u (MinorAmount w) _ _ <- ss])
                mapM_
                  ( \(name, _, wanted) -> case [v | RecordSplit u (MinorAmount _) v _ <- ss, userName u == name] of
                      [actual] -> assertEqual "exact allocation" (MinorAmount wanted) actual
                      _ -> assertFailure "expected exactly one split per user"
                  )
                  expected
              _ -> assertFailure "expected expense"
        _ <- runClient env $ cAddMember tc gid bobId
        wide <- runClient env $ cAddExpense tc gid (expense 100 32767 32767)
        assertSplits [("alice", 32767, 5000), ("bob", 32767, 5000)] wide
        zero <- runClient env $ cAddExpense tc gid (expense 100 0 1)
        assertSplits [("alice", 0, 0), ("bob", 1, 10000)] zero
        large <- runClient env $ cAddExpense tc gid ((expense 100 32767 32767) {ExpenseRequest.amountMinor = MinorAmount 9223372036854775806})
        assertSplits [("alice", 32767, 4611686018427387903), ("bob", 32767, 4611686018427387903)] large
        fetched <- runClient env $ cGetRecord tc gid (extractRecordId large)
        assertSplits [("alice", 32767, 4611686018427387903), ("bob", 32767, 4611686018427387903)] fetched,
      testCase "leaving and reactivation retain historical debt and settlement" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        bobTok <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        let tc = mkTC tok
            bob = mkTC bobTok
            assertHistory report = do
              let bs = [(userName u, total) | Balance {user = u, totalMinor = total} <- concatMap balances (Report.currencies report)]
              assertEqual "historical participants" ["alice", "bob"] (sort $ map fst bs)
              mapM_
                ( \(name, wanted) -> case lookup name bs of
                    Just actual -> assertMoneyMajor ((T.unpack name <> " balance")) (wanted) (actual)
                    Nothing -> assertFailure "missing historical balance"
                )
                [("alice", 60), ("bob", -60)]
              let roleRows =
                    sort
                      [ (userName person, paid, share, received, entitlement, sent, paymentsReceived)
                      | Balance
                          { user = person,
                            expensesPaidMinor = paid,
                            expenseShareMinor = share,
                            incomeReceivedMinor = received,
                            incomeEntitlementMinor = entitlement,
                            paymentsSentMinor = sent,
                            paymentsReceivedMinor = paymentsReceived
                          } <-
                          concatMap balances (Report.currencies report)
                      ]
              assertEqual
                "former and active roles survive membership changes"
                [ ("alice", MinorAmount 9000, MinorAmount 3000, MinorAmount 0, MinorAmount 0, MinorAmount 0, MinorAmount 0),
                  ("bob", MinorAmount 0, MinorAmount 6000, MinorAmount 0, MinorAmount 0, MinorAmount 0, MinorAmount 0)
                ]
                roleRows
              case concatMap settlements (Report.currencies report) of
                [Settlement from to value] -> do
                  assertEqual "debtor" "bob" (userName from)
                  assertEqual "creditor" "alice" (userName to)
                  assertMoneyMajor ("settlement") (60) (value)
                _ -> assertFailure "expected one historical settlement"
        g <- runClient env $ cCreateGroup tc "Historical balances"
        let gid = grpId g
        _ <- runClient env $ cAddMember tc gid bobId
        _ <- runClient env $ cAddExpense tc gid (usdExpense "Dinner" 90 aliceId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (2))])
        before <- runClient env $ cGetReport tc gid
        assertHistory before
        _ <- runClient env $ cDeleteMember bob gid bobId
        active <- runClient env $ cGetGroupById tc gid
        assertEqual "former member absent from active list" ["alice"] (grpMemberNames active)
        runClientExpectStatus env status403 $ cGetGroupById bob gid
        runClientExpectStatus env status403 $ cGetReport bob gid
        reportAfter <- runClient env $ cGetReport tc gid
        assertHistory reportAfter
        assertEqual "complete report unchanged on leave" before reportAfter
        reactivated <- runClient env $ cAddMember tc gid bobId
        assertEqual "reactivated once" ["alice", "bob"] (grpMemberNames reactivated)
        restored <- runClient env $ cGetReport bob gid
        assertHistory restored
        assertEqual "reactivation does not duplicate history" before restored,
      testCase "CSV dates and split counts fail cleanly on user and admin import paths" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        adminTok <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC tok
            admin = mkAdminTC adminTok
            header = "title,amount,paidBy,split,date\n"
            sample = header <> "Dinner,90,@ALICE,\"alice,Bob,@bob\",2026-10-14\n"
            invalidDate = "# ignored comment\n" <> header <> "Valid,10,alice,alice,2026-10-14\n# another comment\nBad,10,alice,alice,not-a-date\n"
            overflow = header <> "Too many,10,alice,\"" <> LBS8.pack (T.unpack $ T.intercalate "," $ replicate 32768 "alice") <> "\",2026-10-14\n"
        mapM_
          ( \(name, importer) -> do
              g <- runClient env $ cCreateGroup tc name
              let gid = grpId g
              _ <- runClient env $ cAddMember tc gid bobId
              empty <- runClient env $ importer gid (CSVData header)
              assertEqual "header-only input imports nothing" [] empty
              runClientExpectStatus env status400 $ importer gid (CSVData invalidDate)
              runClientExpectStatus env status400 $ importer gid (CSVData overflow)
              runClientExpectStatus env status404 $ importer gid (CSVData $ sample <> "Bad payer,10,missing,alice,2026-10-14\n")
              before <- runClient env $ allAdminRecords admin gid
              assertEqual "parse and later-payer failures insert no records" [] before
              imported <- runClient env $ importer gid (CSVData sample)
              assertEqual "five-column CSV imports null record notes" (replicate (length imported) Nothing) (map recordNoteOf imported)
              case imported of
                [ExpenseRecord {title = t, amountMinor = value, date = d, splits = ss}] -> do
                  assertEqual "CSV title" "Dinner" t
                  assertMoneyMajor ("CSV amount") (90) (value)
                  assertEqual "CSV date" (read "2026-10-14") d
                  assertEqual "CSV duplicate names become weights" [("alice", 1), ("bob", 2)] (sort [(userName u, w) | RecordSplit u (MinorAmount w) _ _ <- ss])
                  mapM_
                    ( \(name', wanted) -> case [v | RecordSplit u (MinorAmount _) v _ <- ss, userName u == name'] of
                        [actual] -> assertMoneyMajor ("CSV weighted amount") (wanted) (actual)
                        _ -> assertFailure "missing CSV split"
                    )
                    [("alice", 30), ("bob", 60)]
                _ -> assertFailure "expected exactly one imported expense"
              saved <- runClient env $ withPagination1 (cGetRecords tc) gid defaultPaginationParams
              let canonical :: Record -> Record
                  canonical record@ExpenseRecord {splits = values} = record {StoredRecord.splits = sort values}
                  canonical record = record
              assertEqual "imported response matches stored expense" (map canonical imported) (map canonical $ pageItems saved)
          )
          [("User CSV", cImportGroupCsv tc), ("Admin CSV", cAdminImportGroupCsv admin)],
      testCase "SQL failure on second receipt child rolls back creation and replacement" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env _ _ conn -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        adminTok <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC tok
            admin = mkAdminTC adminTok
            expense title' = usdExpense title' 30 aliceId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (1))]
            batch = [expense "First child", expense "__fail_second_child__"]
            counts = query_ conn "SELECT (SELECT count(*) FROM receipts), (SELECT count(*) FROM records), (SELECT count(*) FROM record_splits)" :: IO [(Int, Int, Int)]
            install = do
              void $ execute_ conn "CREATE FUNCTION integrity_fail_child() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.title = '__fail_second_child__' THEN RAISE EXCEPTION 'forced second child failure' USING ERRCODE = '23514'; END IF; RETURN NEW; END; $$"
              void $ execute_ conn "CREATE TRIGGER integrity_fail_child BEFORE INSERT ON records FOR EACH ROW EXECUTE FUNCTION integrity_fail_child()"
            cleanup = void (execute_ conn "DROP TRIGGER IF EXISTS integrity_fail_child ON records") `finally` void (execute_ conn "DROP FUNCTION IF EXISTS integrity_fail_child()")
        bracket_ (install `onException` cleanup) cleanup $
          mapM_
            ( \(name, create, replace) -> do
                group <- runClient env $ cCreateGroup tc name
                let gid = grpId group
                beforeCreate <- counts
                runClientExpectStatus env status500 $ create gid "Rejected" batch
                afterCreate <- counts
                assertEqual "failed creation leaves no receipt, expense, or split" beforeCreate afterCreate
                receipts <- runClient env $ cAdminGroupReceipts admin gid
                records <- runClient env $ allAdminRecords admin gid
                assertEqual "failed receipt absent" [] receipts
                assertEqual "first child absent after failure" [] records
                original <- runClient env $ create gid "Original note" [expense "Original child"]
                beforeReplace <- counts
                runClientExpectStatus env status500 $ replace gid (receiptId original) (UpdateReceiptRequest "Changed note" batch)
                afterReplace <- counts
                assertEqual "failed replacement restores row counts" beforeReplace afterReplace
                restored <- runClient env $ cGetReceipt tc gid (receiptId original)
                assertEqual "original note, record IDs, and splits survive rollback" original restored
                savedRecords <- runClient env $ allAdminRecords admin gid
                assertEqual "replacement creates no extra child" (receiptRecords original) savedRecords
            )
            [ ("User SQL rollback", \gid note' rs -> cCreateReceipt tc gid (CreateReceiptRequest note' rs), cUpdateReceipt tc),
              ("Admin SQL rollback", \gid note' rs -> cAdminCreateReceipt admin gid (AdminCreateReceiptRequest aliceId note' rs), cAdminUpdateReceipt admin)
            ],
      testCase "SQL failure inserting owner membership rolls back user and admin groups" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env _ _ conn -> do
        tok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        adminTok <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC tok
            admin = mkAdminTC adminTok
            install = do
              void $ execute_ conn "CREATE FUNCTION integrity_fail_membership() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF EXISTS (SELECT 1 FROM groups WHERE id = NEW.group__id AND name = '__fail_owner_membership__') THEN RAISE EXCEPTION 'forced owner membership failure' USING ERRCODE = '23514'; END IF; RETURN NEW; END; $$"
              void $ execute_ conn "CREATE TRIGGER integrity_fail_membership BEFORE INSERT ON group_members FOR EACH ROW EXECUTE FUNCTION integrity_fail_membership()"
            cleanup = void (execute_ conn "DROP TRIGGER IF EXISTS integrity_fail_membership ON group_members") `finally` void (execute_ conn "DROP FUNCTION IF EXISTS integrity_fail_membership()")
        bracket_ (install `onException` cleanup) cleanup $ do
          mapM_
            ( \create -> do
                runClientExpectStatus env status500 create
                groups <- runClient env $ cAdminGroups admin Nothing Nothing Nothing Nothing
                assertEqual "no orphan group is visible" [] (pageItems groups)
                rows <- query_ conn "SELECT (SELECT count(*) FROM groups), (SELECT count(*) FROM group_members)" :: IO [(Int, Int)]
                assertEqual "group and owner membership both roll back" [(0, 0)] rows
            )
            [ cCreateGroup tc "__fail_owner_membership__",
              cAdminCreateGroup admin (AdminCreateGroupRequest ("__fail_owner_membership__") (aliceId) SingleCurrency (CurrencyCode "USD"))
            ]
        healthy <- runClient env $ cCreateGroup tc "Healthy group"
        assertEqual "connection remains usable after rollback" ["alice"] (grpMemberNames healthy),
      testCase "referenced account conflicts preserve history and bulk deletion is atomic" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        aliceTok <- setupUser env "alice"
        aliceId <- fixtureUserId env "alice"
        bobTok <- setupUser env "bob"
        bobId <- fixtureUserId env "bob"
        unusedTok <- setupUser env "unused"
        removableTok <- setupUser env "removable"
        adminTok <- setupAdmin env "admin" "adminpass123"
        let alice = mkTC aliceTok
            bob = mkTC bobTok
            unused = mkTC unusedTok
            removable = mkTC removableTok
            admin = mkAdminTC adminTok
        group <- runClient env $ cCreateGroup alice "Retained ledger"
        let gid = grpId group
        _ <- runClient env $ cAddMember alice gid bobId
        receipt <- runClient env $ cCreateReceipt bob gid (CreateReceiptRequest "History" [usdExpense "Dinner" 90 bobId (read "2026-10-14") [RecordSplitRequest (aliceId) (MinorAmount (1)), RecordSplitRequest (bobId) (MinorAmount (2))]])
        _ <- runClient env $ cDeleteMember bob gid bobId
        bobProfile <- runClient env $ cGetMe bob
        UserProfile (User unusedId _ _) _ _ <- runClient env $ cGetMe unused
        UserProfile (User removableId _ _) _ _ <- runClient env $ cGetMe removable
        ledgerBefore <- runClient env $ allAdminRecords admin gid
        reportBefore <- runClient env $ cGetReport alice gid
        runClientExpectStatus env status409 $ cDeleteMe alice
        runClientExpectStatus env status409 $ cDeleteMe bob
        runClientExpectStatus env status409 $ cAdminDeleteUser admin bobId
        runClientExpectStatus env status409 $ cAdminBulkDeleteUsers admin (BulkRequest [unusedId, bobId])
        retainedUnused <- runClient env $ cGetMe unused
        retainedBob <- runClient env $ cGetMe bob
        assertEqual "first bulk account restored" (User unusedId "unused" "unused") (profileUser retainedUnused)
        assertEqual "referenced account retained" (profileUser bobProfile) (profileUser retainedBob)
        _ <- runClient env $ cDeleteMe removable
        accounts <- runClient env $ cAdminUsers admin Nothing Nothing Nothing Nothing
        assertBool "unused account deletion succeeds" (removableId `notElem` [uid | Oidc.AdminUserSummary (UserProfile (User uid _ _) _ _) _ _ _ <- pageItems accounts])
        restoredReceipt <- runClient env $ cGetReceipt alice gid (receiptId receipt)
        ledgerAfter <- runClient env $ allAdminRecords admin gid
        reportAfter <- runClient env $ cGetReport alice gid
        assertEqual "receipt history retained" receipt restoredReceipt
        assertEqual "expense and split history retained" (sort ledgerBefore) (sort ledgerAfter)
        assertEqual "balances and settlements retained" reportBefore reportAfter
    ]

identityRequest :: Int -> Manager -> Maybe Text -> B8.ByteString -> String -> Value -> IO (HC.Response LBS8.ByteString)
identityRequest port manager token method path body = do
  request <- localRequest port path
  httpLbs (request {HC.method = method, HC.requestBody = HC.RequestBodyLBS (encode body), HC.requestHeaders = [("Content-Type", "application/json")] <> maybe [] (\value -> [("Authorization", "Bearer " <> encodeUtf8 value)]) token}) manager

assertPrivateAbsent :: Value -> Assertion
assertPrivateAbsent value = do
  let body = LBS8.toStrict $ encode value
  forM_ ["\"email\"", "\"emailVerified\"", "password", "externalIdentities", "issuer", "subject"] $ \field ->
    assertBool ("public response must omit " <> B8.unpack field) (not $ field `B8.isInfixOf` body)

identityTests :: TestTree
identityTests =
  testGroup
    "User identity"
    [ testCase "required identity fields and reserved verification fields are rejected" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        adminToken <- setupAdmin env "admin" "adminpass123"
        let registration values = object values
            fields = ["registerUsername" .= ("alice" :: Text), "registerDisplayName" .= ("Alex" :: Text), "registerEmail" .= ("alice@example.test" :: Text), "registerPassword" .= ("password123" :: Text)]
            adminFields = ["createUsername" .= ("alice" :: Text), "createDisplayName" .= ("Alex" :: Text), "createEmail" .= ("alice@example.test" :: Text), "createPassword" .= ("password123" :: Text), "createMode" .= ("local" :: Text)]
        forM_ [0 .. 3] $ \index -> do
          response <- identityRequest port manager Nothing "POST" "/auth/register" (registration $ take index fields <> drop (index + 1) fields)
          assertEqual "missing registration field" status400 (HC.responseStatus response)
          adminResponse <- identityRequest port manager (Just adminToken) "POST" "/admin/users" (object $ take index adminFields <> drop (index + 1) adminFields)
          assertEqual "missing admin creation field" status400 (HC.responseStatus adminResponse)
        forM_ ["emailVerified", "registerEmailVerified"] $ \field -> do
          response <- identityRequest port manager Nothing "POST" "/auth/register" (object $ fields <> [field .= True])
          assertEqual "registration cannot choose verification" status400 (HC.responseStatus response)
        forM_ ["emailVerified", "createEmailVerified"] $ \field -> do
          response <- identityRequest port manager (Just adminToken) "POST" "/admin/users" (object $ adminFields <> [field .= True])
          assertEqual "admin creation cannot choose verification" status400 (HC.responseStatus response),
      testCase "canonical handles and email collide but display names do not" $ withTestApp $ \env -> do
        first <- runClient env $ register_ (RegisterRequest " @ALIce " "  Alex 李  " " Alice@Example.Test " "password123")
        second <- runClient env $ register_ (RegisterRequest "bob" "Alex 李" "bob@example.test" "password123")
        assertEqual "canonical public identity" ("alice", "Alex 李") (case first of User _ handle label -> (handle, label))
        assertBool "duplicate display names have distinct UUIDs" (userIdOf first /= userIdOf second)
        runClientExpectStatus env status409 $ register_ (RegisterRequest "ALICE" "Other" "other@example.test" "password123")
        runClientExpectStatus env status409 $ register_ (RegisterRequest "other" "Other" "ALICE@EXAMPLE.TEST" "password123")
        forM_ ["alice", "@ALICE", "  Alice@EXAMPLE.TEST  "] $ \identifier -> do
          response <- runClient env $ login_ (LoginRequest identifier "password123")
          assertEqual "identifier targets same UUID" (userIdOf first) (userIdOf $ loginUser response)
          profile <- runClient env $ cGetMe (mkTC $ response.accessToken)
          assertEqual "email normalized, not verified" (UserProfile first "alice@example.test" False) profile,
      testCase "normal-user validation rejects invalid identity and password boundaries" $ withTestApp $ \env -> do
        forM_ ["", "@", "-alice", ".alice", "two words", "élise", "alice@example.test", T.replicate 51 "a"] $ \handle ->
          runClientExpectStatus env status400 $ register_ (RegisterRequest handle "Alex" "valid@example.test" "password123")
        forM_ ["", "   ", "Alex\nName", T.replicate 101 "李"] $ \label ->
          runClientExpectStatus env status400 $ register_ (RegisterRequest "valid" label "valid@example.test" "password123")
        forM_ ["", "not-email", "a b@example.test", "é@example.test", T.replicate 250 "a" <> "@example.test"] $ \emailAddress ->
          runClientExpectStatus env status400 $ register_ (RegisterRequest "valid" "Alex" emailAddress "password123")
        runClientExpectStatus env status400 $ register_ (RegisterRequest "valid" "Alex" "valid@example.test" "short")
        user <- runClient env $ register_ (RegisterRequest (T.replicate 50 "a") (T.replicate 100 "李") "valid@example.test" "password123")
        assertEqual "Unicode display-name boundary retained" (T.replicate 100 "李") (case user of User _ _ label -> label),
      testCase "profile saves preserve UUID private state and existing token; conflicts roll back" $ withTestAppServer Nothing id $ \env _ _ conn -> do
        token <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC token
        before <- runClient env $ cGetMe tc
        void $ execute_ conn "UPDATE users SET email_verified = TRUE WHERE username = 'alice'"
        updated <- runClient env $ cUpdateMe tc (UpdateProfileRequest " @ALICE.NEW " "  New 李  ")
        let UserProfile original emailAddress _ = before
            expected = UserProfile (User (userIdOf original) "alice.new" "New 李") emailAddress True
        assertEqual "only public labels change" expected updated
        unchanged <- runClient env $ cUpdateMe tc (UpdateProfileRequest "ALICE.NEW" "New 李")
        assertEqual "canonical same-value save succeeds" expected unchanged
        group <- runClient env $ cCreateGroup tc "After rename"
        assertEqual "old token uses current identity" "alice.new" (grpOwnerName group)
        runClientExpectStatus env status409 $ cUpdateMe tc (UpdateProfileRequest "bob" "Must roll back")
        afterConflict <- runClient env $ cGetMe tc
        assertEqual "conflicting handle rolls back display name and private fields" expected afterConflict
        runClientExpectStatus env status400 $ cUpdateMe tc (UpdateProfileRequest "invalid handle" "Must roll back")
        forM_ ["alice.new", "ALICE@EXAMPLE.TEST"] $ \identifier -> do
          loginResponse <- runClient env $ login_ (LoginRequest identifier "password123")
          assertEqual "rename preserves password UUID" (userIdOf original) (userIdOf $ loginUser loginResponse)
        runClientExpectStatus env status401 $ login_ (LoginRequest "alice" "password123"),
      testCase "profile PUT rejects private fields and deleted token cannot update replacement" $ withTestAppServer Nothing id $ \env port manager _ -> do
        token <- setupUser env "alice"
        let tc = mkTC token
        before <- runClient env $ cGetMe tc
        forM_ [["email" .= ("new@example.test" :: Text)], ["emailVerified" .= True], ["registerEmailVerified" .= True]] $ \privateFields -> do
          response <- identityRequest port manager (Just token) "PUT" "/users/me" (object $ ["username" .= ("changed" :: Text), "displayName" .= ("Changed" :: Text)] <> privateFields)
          assertEqual "unknown private fields rejected" status400 (HC.responseStatus response)
          current <- runClient env $ cGetMe tc
          assertEqual "rejected PUT writes nothing" before current
        _ <- runClient env $ cDeleteMe tc
        replacementToken <- setupUser env "alice"
        runClientExpectStatus env status401 $ cUpdateMe tc (UpdateProfileRequest "stolen" "Stolen")
        replacement <- runClient env $ cGetMe (mkTC replacementToken)
        assertBool "released handle has a new UUID" (userIdOf (profileUser before) /= userIdOf (profileUser replacement))
        assertEqual "replacement unmodified" "alice" (userName $ profileUser replacement),
      testCase "passwordless credentials fail generically and provider identities enforce uniqueness and cascade" $ withTestAppServer Nothing id $ \env _ _ conn -> do
        _ <- setupUser env "alice"
        [Only passwordlessId] <- query_ conn "INSERT INTO users (id, username, display_name, email, password_hash) VALUES (gen_random_uuid(), 'passwordless', 'Alex', 'passwordless@example.test', NULL) RETURNING id::text" :: IO [Only Text]
        [Only aliceUUID] <- query_ conn "SELECT id::text FROM users WHERE username = 'alice'" :: IO [Only Text]
        forM_ [("bad handle!", "password123"), ("missing", "password123"), ("alice", "wrongpassword"), ("passwordless", "password123"), ("PASSWORDLESS@EXAMPLE.TEST", "password123"), ("invalid@@example.test", "password123")] $ \(identifier, password) -> do
          result <- runClientM (login_ $ LoginRequest identifier password) env
          case result of
            Left (FailureResponse _ response) -> do
              assertEqual "generic credentials status" status401 (responseStatusCode response)
              assertEqual "generic credentials body" "Invalid username/email or password" (responseBody response)
            _ -> assertFailure "Expected credential failure"
        withTransaction conn $ do
          void $ execute conn "INSERT INTO external_identities (issuer, subject, user__id, linked_at) VALUES ('issuer-a', 'subject', ?::uuid, now())" (Only passwordlessId)
        duplicate <- try (withTransaction conn $ execute conn "INSERT INTO external_identities (issuer, subject, user__id, linked_at) VALUES ('issuer-a', 'subject', ?::uuid, now())" (Only aliceUUID)) :: IO (Either SqlError Int64)
        case duplicate of
          Left sqlError -> assertEqual "issuer/subject uniqueness" "23505" (sqlState sqlError)
          Right _ -> assertFailure "Duplicate provider subject was accepted"
        duplicateUser <- try (withTransaction conn $ execute conn "INSERT INTO external_identities (issuer, subject, user__id, linked_at) VALUES ('issuer-b', 'other-subject', ?::uuid, now())" (Only passwordlessId)) :: IO (Either SqlError Int64)
        case duplicateUser of
          Left sqlError -> assertEqual "one external identity per account" "23505" (sqlState sqlError)
          Right _ -> assertFailure "A second identity was attached to the same account"
        identities <- query conn "SELECT issuer, subject FROM external_identities WHERE user__id = ?::uuid ORDER BY issuer, subject" (Only passwordlessId) :: IO [(Text, Text)]
        assertEqual "only the original identity remains" [("issuer-a", "subject")] identities
        verification <- query conn "SELECT email_verified FROM users WHERE id = ?::uuid" (Only passwordlessId) :: IO [Only Bool]
        assertEqual "passwordless defaults unverified" [Only False] verification
        withTransaction conn $ void $ execute conn "DELETE FROM users WHERE id = ?::uuid" (Only passwordlessId)
        remaining <- query_ conn "SELECT count(*) FROM external_identities" :: IO [Only Int]
        assertEqual "deletion cascades provider identities" [Only 0] remaining,
      testCase "admin creation shares validation and exposes only authorized private profiles" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        adminToken <- setupAdmin env "admin" "adminpass123"
        let admin = mkAdminTC adminToken
        details <- runClient env $ cAdminCreateUser admin (AdminCreateUserRequest " @ALICE " "  Alex 李 " " ALICE@EXAMPLE.TEST " AdminCreateLocal (Just "password123"))
        let profile = details.profile
            UserProfile public emailAddress verified = profile
        assertEqual "admin normalized private values" ("alice", "alice@example.test", False) (userName public, emailAddress, verified)
        runClientExpectStatus env status409 $ cAdminCreateUser admin (AdminCreateUserRequest "bob" "Alex" "alice@example.test" AdminCreateLocal (Just "password123"))
        runClientExpectStatus env status409 $ cAdminCreateUser admin (AdminCreateUserRequest "ALICE" "Alex" "bob@example.test" AdminCreateLocal (Just "password123"))
        runClientExpectStatus env status400 $ cAdminCreateUser admin (AdminCreateUserRequest "bad handle" "Alex" "valid@example.test" AdminCreateLocal (Just "password123"))
        runClientExpectStatus env status400 $ cAdminCreateUser admin (AdminCreateUserRequest "bob" "" "valid@example.test" AdminCreateLocal (Just "password123"))
        runClientExpectStatus env status400 $ cAdminCreateUser admin (AdminCreateUserRequest "bob" "Alex" "not-email" AdminCreateLocal (Just "password123"))
        users <- runClient env $ cAdminUsers admin Nothing Nothing Nothing Nothing
        assertEqual "admin list includes private profiles" [profile] (map (\summary -> summary.profile) $ pageItems users)
        loginResponse <- runClient env $ login_ (LoginRequest "alice@example.test" "password123")
        let token = loginResponse.accessToken
        me <- identityRequest port manager (Just token) "GET" "/users/me" Null
        assertEqual "self profile contains email" (Right $ toJSON profile) (eitherDecode $ HC.responseBody me)
        assertPrivateAbsent $ toJSON loginResponse,
      testCase "open UUID drafts cannot retarget a released handle and public reads use current labels" $ withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
        ownerToken <- setupUser env "owner"
        memberToken <- setupUser env "member"
        adminToken <- setupAdmin env "admin" "adminpass123"
        let owner = mkTC ownerToken
            member = mkTC memberToken
        ownerProfile <- runClient env $ cGetMe owner
        memberProfile <- runClient env $ cGetMe member
        let ownerId = userIdOf $ profileUser ownerProfile
            memberId = userIdOf $ profileUser memberProfile
            draft = usdExpense "Saved draft" 90 memberId (read "2026-10-14") [RecordSplitRequest (ownerId) (MinorAmount (1)), RecordSplitRequest (memberId) (MinorAmount (2))]
            transferDraft = usdTransfer (10) (memberId) (ownerId) ((read "2026-10-14"))
        group <- runClient env $ cCreateGroup owner "Rename safe"
        let gid@(GroupId uuid) = grpId group
        _ <- runClient env $ cAddMember owner gid memberId
        expense <- runClient env $ cAddExpense owner gid draft
        transfer <- runClient env $ cAddTransfer owner gid transferDraft
        receipt <- runClient env $ cCreateReceipt member gid (CreateReceiptRequest "Before rename" [draft])
        reportBefore <- runClient env $ cGetReport owner gid
        _ <- runClient env $ cUpdateMe member (UpdateProfileRequest "renamed" "Alex")
        replacement <- runClient env $ register_ (RegisterRequest "member" "Alex" "replacement@example.test" "password123")
        assertBool "released handle is a separate UUID" (userIdOf replacement /= memberId)
        reportAfter <- runClient env $ cGetReport owner gid
        let totals report = sort [(userIdOf value, total) | Balance {user = value, totalMinor = total} <- concatMap balances (Report.currencies report)]
        assertEqual "renaming leaves balances unchanged" (totals reportBefore) (totals reportAfter)
        forM_ [expense, transfer] $ \record -> do
          fresh <- runClient env $ cGetRecord owner gid (extractRecordId record)
          case fresh of
            ExpenseRecord {paidBy = payer} -> assertEqual "historical expense follows UUID" (memberId, "renamed") (userIdOf payer, userName payer)
            TransferRecord {paidBy = payer} -> assertEqual "historical transfer follows UUID" (memberId, "renamed") (userIdOf payer, userName payer)
            IncomeRecord {} -> assertFailure "Expected historical expense or transfer"
        oldReceipt <- runClient env $ cGetReceipt owner gid (receiptId receipt)
        assertEqual "historical uploader follows UUID" (memberId, "renamed") (userIdOf $ uploadedBy oldReceipt, userName $ uploadedBy oldReceipt)
        saved <- runClient env $ cAddExpense owner gid draft
        case saved of
          ExpenseRecord {paidBy = payer, splits = values} -> do
            assertEqual "open payer remains original" memberId (userIdOf payer)
            assertEqual "open splits remain original UUIDs" (sort [ownerId, memberId]) (sort [userIdOf value | RecordSplit value (MinorAmount _) _ _ <- values])
            assertBool "replacement is not selected" (userIdOf replacement `notElem` [userIdOf value | RecordSplit value (MinorAmount _) _ _ <- values])
          _ -> assertFailure "Expected saved expense"
        savedTransfer <- runClient env $ cAddTransfer owner gid transferDraft
        case savedTransfer of
          TransferRecord {paidBy = payer, transferTo = recipient} ->
            assertEqual "open transfer remains UUID-bound" (memberId, ownerId) (userIdOf payer, userIdOf recipient)
          _ -> assertFailure "Expected saved transfer"
        savedReceipt <- runClient env $ cCreateReceipt member gid (CreateReceiptRequest "UUID child" [draft])
        assertEqual "renamed token preserves uploader UUID" memberId (userIdOf $ uploadedBy savedReceipt)
        case receiptRecords savedReceipt of
          [ExpenseRecord {paidBy = payer}] -> assertEqual "receipt child retains selected payer UUID" memberId (userIdOf payer)
          _ -> assertFailure "Expected one receipt expense"
        runClientExpectStatus env status400 $ cTransferOwnership owner gid (userIdOf replacement)
        transferred <- runClient env $ cTransferOwnership owner gid memberId
        assertEqual "previously selected owner UUID remains original" memberId (case transferred of Group {owner = value} -> userIdOf value)
        forM_ [(ownerToken, "/groups/" <> show uuid), (ownerToken, "/groups/" <> show uuid <> "/records"), (ownerToken, "/groups/" <> show uuid <> "/receipts"), (ownerToken, "/groups/" <> show uuid <> "/report")] $ \(token, path) -> do
          response <- identityRequest port manager (Just token) "GET" path Null
          assertEqual "public endpoint status" status200 (HC.responseStatus response)
          case eitherDecode (HC.responseBody response) of
            Right value -> assertPrivateAbsent value
            Left message -> assertFailure message
        adminResponse <- identityRequest port manager (Just adminToken) "GET" "/admin/users" Null
        assertEqual "admin directory remains private" status200 (HC.responseStatus adminResponse)
        assertBool "admin directory includes private email" ("replacement@example.test" `B8.isInfixOf` LBS8.toStrict (HC.responseBody adminResponse))
    ]

memberCandidateTests :: TestTree
memberCandidateTests =
  testGroup
    "Member candidates"
    [ testCase "owner searches handles and duplicate display names with stable paging" $ withTestApp $ \env -> do
        token <- setupUser env "owner"
        alpha <- runClient env $ register_ (RegisterRequest "alpha" "Alex 李" "alpha@example.test" "password123")
        beta <- runClient env $ register_ (RegisterRequest "beta" "Alex 李" "beta@example.test" "password123")
        handleMatch <- runClient env $ register_ (RegisterRequest "alexhandle" "Other name" "handle@example.test" "password123")
        let owner = mkTC token
        group <- runClient env $ cCreateGroup owner "Search"
        let candidates = cMemberCandidates owner (grpId group)
        first <- runClient env $ candidates (Just "  aLeX 李  ") (Just 0) (Just 1) (Just "username")
        assertEqual "first duplicate display name" [alpha] (pageItems first)
        let PageInfo firstOffset firstLimit firstTotal firstNext firstPrevious = pageMeta first
        assertEqual "matching count and first-page flags" (0, 1, 2, True, False) (firstOffset, firstLimit, firstTotal, firstNext, firstPrevious)
        second <- runClient env $ candidates (Just "alex 李") (Just 1) (Just 1) (Just "username")
        assertEqual "second duplicate retains distinct UUID" [beta] (pageItems second)
        let PageInfo _ _ secondTotal secondNext secondPrevious = pageMeta second
        assertEqual "last-page count and flags" (2, False, True) (secondTotal, secondNext, secondPrevious)
        descending <- runClient env $ candidates (Just "alex 李") Nothing Nothing (Just "-username")
        assertEqual "descending username order" [beta, alpha] (pageItems descending)
        handleOnly <- runClient env $ candidates (Just "  @ALeX  ") Nothing Nothing Nothing
        assertEqual "at-prefix never searches display names" [handleMatch] (pageItems handleOnly)
        substring <- runClient env $ candidates (Just "LPH") Nothing Nothing Nothing
        assertEqual "handle substring is case-insensitive" [alpha] (pageItems substring)
        noEmail <- runClient env $ candidates (Just "alpha@example.test") Nothing Nothing Nothing
        assertEqual "email is not searchable" [] (pageItems noEmail)
        let PageInfo _ _ noEmailTotal _ _ = pageMeta noEmail
        assertEqual "empty search has zero total" 0 noEmailTotal,
      testCase "active members are excluded before pagination and counts; blank queries browse" $ withTestApp $ \env -> do
        token <- setupUser env "aowner"
        _ <- setupUser env "bactive"
        activeId <- fixtureUserId env "bactive"
        _ <- setupUser env "carol"
        _ <- setupUser env "dave"
        let owner = mkTC token
        group <- runClient env $ cCreateGroup owner "Eligible users"
        let gid = grpId group
            candidates = cMemberCandidates owner gid
        _ <- runClient env $ cAddMember owner gid activeId
        first <- runClient env $ candidates Nothing (Just 0) (Just 1) (Just "username")
        assertEqual "active prefix does not consume page" ["carol"] (map userName $ pageItems first)
        let PageInfo off lim total next previous = pageMeta first
        assertEqual "eligible-only metadata" (0, 1, 2, True, False) (off, lim, total, next, previous)
        second <- runClient env $ candidates (Just "   ") (Just 1) (Just 1) (Just "username")
        assertEqual "blank query browses second eligible user" ["dave"] (map userName $ pageItems second)
        defaults <- runClient env $ candidates Nothing Nothing Nothing Nothing
        assertEqual "default browse order" ["carol", "dave"] (map userName $ pageItems defaults)
        let PageInfo defaultOffset defaultLimit defaultTotal _ _ = pageMeta defaults
        assertEqual "existing defaults" (0, 20, 2) (defaultOffset, defaultLimit, defaultTotal)
        clamped <- runClient env $ candidates Nothing (Just (-5)) (Just 0) Nothing
        let PageInfo clampedOffset clampedLimit clampedTotal _ _ = pageMeta clamped
        assertEqual "negative offset and zero limit clamp" (0, 1, 2) (clampedOffset, clampedLimit, clampedTotal)
        large <- runClient env $ candidates Nothing Nothing (Just 101) Nothing
        let PageInfo _ largeLimit _ _ _ = pageMeta large
        assertEqual "limit caps at 100" 100 largeLimit
        beyond <- runClient env $ candidates Nothing (Just 10) (Just 1) Nothing
        assertEqual "past-end page is empty" [] (pageItems beyond)
        let PageInfo _ _ beyondTotal beyondNext beyondPrevious = pageMeta beyond
        assertEqual "past-end metadata retains eligible count" (2, False, True) (beyondTotal, beyondNext, beyondPrevious),
      testCase "inactive members are eligible and UUID selection reactivates membership" $ withTestApp $ \env -> do
        token <- setupUser env "owner"
        _ <- setupUser env "former"
        formerId <- fixtureUserId env "former"
        let owner = mkTC token
        group <- runClient env $ cCreateGroup owner "Reactivation"
        let gid = grpId group
            candidates = cMemberCandidates owner gid Nothing Nothing Nothing Nothing
        _ <- runClient env $ cAddMember owner gid formerId
        active <- runClient env candidates
        assertEqual "all registered users active means no choices" [] (pageItems active)
        _ <- runClient env $ cDeleteMember owner gid formerId
        eligible <- runClient env candidates
        assertEqual "inactive membership remains selectable by UUID" [formerId] (map userIdOf $ pageItems eligible)
        restored <- runClient env $ cAddMember owner gid formerId
        assertEqual "reactivation restores active roster" ["former", "owner"] (grpMemberNames restored)
        remaining <- runClient env candidates
        assertEqual "reactivated user disappears from choices" [] (pageItems remaining)
        runClientExpectStatus env status409 $ cAddMember owner gid formerId,
      testCase "only the current owner can discover users, including empty searches" $ withTestAppServer Nothing id $ \env port manager _ -> do
        ownerToken <- setupUser env "owner"
        memberToken <- setupUser env "member"
        outsiderToken <- setupUser env "outsider"
        memberId <- fixtureUserId env "member"
        let owner = mkTC ownerToken
            member = mkTC memberToken
            outsider = mkTC outsiderToken
        group <- runClient env $ cCreateGroup owner "Authorization"
        let gid@(GroupId uuid) = grpId group
            path = "/groups/" <> show uuid <> "/member-candidates"
        _ <- runClient env $ cAddMember owner gid memberId
        forM_ [Nothing, Just "", Just "no matching users"] $ \search -> do
          runClientExpectStatus env status403 $ cMemberCandidates member gid search Nothing Nothing Nothing
          runClientExpectStatus env status403 $ cMemberCandidates outsider gid search Nothing Nothing Nothing
        anonymous <- identityRequest port manager Nothing "GET" path Null
        assertEqual "missing bearer token" status401 (HC.responseStatus anonymous)
        transferred <- runClient env $ cTransferOwnership owner gid memberId
        assertEqual "new owner is active" "member" (grpOwnerName transferred)
        runClientExpectStatus env status403 $ cMemberCandidates owner gid Nothing Nothing Nothing Nothing
        allowed <- runClient env $ cMemberCandidates member gid Nothing Nothing Nothing Nothing
        assertEqual "new owner can browse" ["outsider"] (map userName $ pageItems allowed)
        _ <- runClient env $ cDeleteGroupById member gid
        forM_ [owner, member, outsider] $ \caller ->
          runClientExpectStatus env status404 $ cMemberCandidates caller gid Nothing Nothing Nothing Nothing,
      testCase "actual directory JSON contains public identities only" $ withTestAppServer Nothing id $ \env port manager _ -> do
        token <- setupUser env "owner"
        candidate <- runClient env $ register_ (RegisterRequest "candidate" "Alex 李" "private-mail@example.test" "password123")
        let owner = mkTC token
        group <- runClient env $ cCreateGroup owner "Privacy"
        let gid@(GroupId uuid) = grpId group
        expected <- runClient env $ cMemberCandidates owner gid Nothing (Just 0) (Just 20) (Just "username")
        assertEqual "expected public candidate" [candidate] (pageItems expected)
        response <- identityRequest port manager (Just token) "GET" ("/groups/" <> show uuid <> "/member-candidates?offset=0&limit=20&sort=username") Null
        assertEqual "raw directory status" status200 (HC.responseStatus response)
        case eitherDecode (HC.responseBody response) of
          Right value -> do
            assertEqual "wire response agrees with typed public page" (toJSON expected) value
            assertPrivateAbsent value
          Left message -> assertFailure message
        assertBool "private email value is absent" (not $ "private-mail@example.test" `B8.isInfixOf` LBS8.toStrict (HC.responseBody response))
    ]

currencyPolicyTests :: TestTree
currencyPolicyTests =
  testGroup
    "Currency policy"
    [ testCase "create defaults and strict request shapes" $ withTestAppServer Nothing id $ \env port manager _ -> do
        token <- setupUser env "owner"
        response <- identityRequest port manager (Just token) "POST" "/groups" (object ["groupName" .= ("Default" :: Text)])
        assertEqual "default create status" status200 (HC.responseStatus response)
        case (eitherDecode (HC.responseBody response) :: Either String Group) of
          Right group -> do
            assertEqual "default mode" SingleCurrency group.currencyMode
            assertEqual "default currency" (CurrencyCode "USD") group.defaultCurrency.code
            assertEqual "initial lock" False group.currencyLocked
          Left message -> assertFailure message
        forM_ [String "Old string body", object ["groupName" .= ("Unknown" :: Text), "defaultCurrency" .= ("ZZZ" :: Text)], object ["groupName" .= ("Unknown field" :: Text), "extra" .= True]] $ \body -> do
          rejected <- identityRequest port manager (Just token) "POST" "/groups" body
          assertEqual "invalid group body" status400 (HC.responseStatus rejected),
      testCase "empty receipts do not lock; first record locks permanently and settings conflicts are atomic" $ withTestAppServer Nothing id $ \env port manager _ -> do
        token <- setupUser env "owner"
        ownerId <- fixtureUserId env "owner"
        let tc = mkTC token
        group <- runClient env $ cCreateGroup tc "Policy"
        let gid@(GroupId uuid) = grpId group
            path = "/groups/" <> show uuid
            update name mode currency = identityRequest port manager (Just token) "PUT" path (toJSON $ UpdateGroupRequest name mode (CurrencyCode currency))
        _ <- runClient env $ cCreateReceipt tc gid (CreateReceiptRequest "Empty" [])
        empty <- runClient env $ cGetGroupById tc gid
        assertEqual "empty receipt leaves policy unlocked" False empty.currencyLocked
        changed <- update "Before first record" MultiCurrency "EUR"
        assertEqual "unlocked policy changes" status200 (HC.responseStatus changed)
        invalid <- runClientM (cAddExpense tc gid ((usdExpense "Invalid" 0 ownerId (read "2026-10-14") [RecordSplitRequest ownerId (MinorAmount 1)]) {ExpenseRequest.currency = CurrencyCode "EUR"})) env
        case invalid of
          Left (FailureResponse _ response) -> assertEqual "invalid expense" status400 (responseStatusCode response)
          _ -> assertFailure "expected invalid expense rejection"
        stillEmpty <- runClient env $ cGetGroupById tc gid
        assertEqual "failed validation leaves policy unlocked" False stillEmpty.currencyLocked
        record <- runClient env $ cAddExpense tc gid ((usdExpense "First" 10 ownerId (read "2026-10-14") [RecordSplitRequest ownerId (MinorAmount 1)]) {ExpenseRequest.currency = CurrencyCode "EUR"})
        locked <- runClient env $ cGetGroupById tc gid
        assertEqual "financial record locks" True locked.currencyLocked
        rejected <- update "Must not rename" SingleCurrency "USD"
        assertEqual "locked policy conflict" status409 (HC.responseStatus rejected)
        unchanged <- runClient env $ cGetGroupById tc gid
        assertEqual "conflict leaves every field unchanged" locked unchanged
        renamed <- update "Renaming allowed" MultiCurrency "EUR"
        assertEqual "same-policy rename" status200 (HC.responseStatus renamed)
        _ <- runClient env $ cDeleteRecord tc gid (extractRecordId record)
        afterDelete <- runClient env $ cGetGroupById tc gid
        assertEqual "deleting final record never unlocks" True afterDelete.currencyLocked
        assertEqual "rename persisted" "Renaming allowed" (grpName afterDelete),
      testCase "every user and admin financial creation path locks currency policy" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        token <- setupUser env "owner"
        ownerId <- fixtureUserId env "owner"
        _ <- setupUser env "member"
        memberId <- fixtureUserId env "member"
        adminToken <- setupAdmin env "admin" "adminpass123"
        let tc = mkTC token
            admin = mkAdminTC adminToken
            expense = usdExpense "Lock" 10 ownerId (read "2026-10-14") [RecordSplitRequest ownerId (MinorAmount 1)]
            transfer = usdTransfer 10 ownerId memberId (read "2026-10-14")
            csv = CSVData "title,amount,paidBy,split,date\nLock,10,owner,owner,2026-10-14\n"
            writers =
              [ ("user expense", \gid -> void $ cAddExpense tc gid expense),
                ("user transfer", \gid -> void $ cAddTransfer tc gid transfer),
                ("user receipt", \gid -> void $ cCreateReceipt tc gid (CreateReceiptRequest "Lock" [expense])),
                ("user import", \gid -> void $ cImportGroupCsv tc gid csv),
                ("admin expense", \gid -> void $ cAdminAddExpense admin gid expense),
                ("admin transfer", \gid -> void $ cAdminAddTransfer admin gid transfer),
                ("admin receipt", \gid -> void $ cAdminCreateReceipt admin gid (AdminCreateReceiptRequest ownerId "Lock" [expense])),
                ("admin import", \gid -> void $ cAdminImportGroupCsv admin gid csv)
              ]
        forM_ writers $ \(name, writer) -> do
          group <- runClient env $ cCreateGroup tc name
          let gid = grpId group
          _ <- runClient env $ cAddMember tc gid memberId
          runClient env $ writer gid
          saved <- runClient env $ cGetGroupById tc gid
          assertEqual (T.unpack name <> " locks") True saved.currencyLocked,
      testCase "a policy update winning the first-record race rejects the stale write" $ withTestApp $ \env -> do
        token <- setupUser env "owner"
        ownerId <- fixtureUserId env "owner"
        let tc = mkTC token
        group <- runClient env $ cCreateGroup tc "Race"
        let gid@(GroupId uuid) = grpId group
            expense = usdExpense "Must not persist" 10 ownerId (read "2026-10-14") [RecordSplitRequest ownerId (MinorAmount 1)]
        connStr <- fromMaybe "dbname=jizhang_test" . fmap pack <$> lookupEnv "TEST_DATABASE_URL"
        done <- newEmptyMVar
        bracket (connectPostgreSQL connStr) close $ \locker -> do
          withTransaction locker $ do
            _ <- query locker "SELECT id FROM groups WHERE id = ? FOR UPDATE" (Only uuid) :: IO [Only UUID.UUID]
            _ <- forkIO $ runClientM (cAddExpense tc gid expense) env >>= putMVar done
            let awaitBlocked 0 = assertFailure "financial write did not reach conditional row lock"
                awaitBlocked attempts = do
                  -- Activity snapshots are cached for the lock-holder transaction.
                  _ <- query_ locker "SELECT 1 FROM pg_stat_clear_snapshot()" :: IO [Only Int]
                  rows <- query_ locker "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE pid <> pg_backend_pid() AND datname = current_database() AND wait_event_type = 'Lock' AND query LIKE '%currency_locked%')" :: IO [Only Bool]
                  case rows of
                    [Only True] -> pure ()
                    _ -> threadDelay 10000 >> awaitBlocked (attempts - 1)
            awaitBlocked (500 :: Int)
            _ <- execute locker "UPDATE groups SET default_currency = 'EUR' WHERE id = ?" (Only uuid)
            pure ()
          result <- takeMVar done
          case result of
            Left (FailureResponse _ response) -> assertEqual "stale policy write" status409 (responseStatusCode response)
            Left err -> assertFailure (show err)
            Right _ -> assertFailure "stale first record must not save"
        saved <- runClient env $ cGetGroupById tc gid
        assertEqual "winning policy preserved" (CurrencyCode "EUR") saved.defaultCurrency.code
        assertEqual "rejected write did not lock" False saved.currencyLocked
        records <- runClient env $ cAdminlessRecords tc gid
        assertEqual "rejected write did not persist" [] records
    ]
  where
    cAdminlessRecords tc gid = pageItems <$> withPagination1 (cGetRecords tc) gid defaultPaginationParams

recordNoteOf :: Record -> Maybe Text
recordNoteOf record = record.note

canonicalRecord :: Record -> Record
canonicalRecord record@ExpenseRecord {splits = shares} = record {StoredRecord.splits = sort shares}
canonicalRecord record@IncomeRecord {splits = shares} = record {StoredRecord.splits = sort shares}
canonicalRecord record = record

recordNoteTests :: TestTree
recordNoteTests =
  testGroup
    "Notes"
    [ testCase "all kinds preserve exact notes across public/admin CRUD and reject invalid notes atomically" $
        withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager _ -> do
          token <- setupUser env "alice"
          alice <- fixtureUserId env "alice"
          _ <- setupUser env "bob"
          bob <- fixtureUserId env "bob"
          adminToken <- setupAdmin env "admin" "adminpass123"
          let tc = mkTC token
              admin = mkAdminTC adminToken
              exact = "  Invoice 42\n会议 <b>plain</b>  " :: Text
              changed = exact <> "\nUpdated"
              day = read "2026-10-14"
              cases =
                [ ("expense", toJSON $ ExpenseRecordRequest "Expense" Nothing (MinorAmount 9001) (CurrencyCode "USD") alice day ExactSplit [RecordSplitRequest alice (MinorAmount 3000), RecordSplitRequest bob (MinorAmount 6001)] Nothing),
                  ("income", toJSON $ IncomeRecordRequest "Income" Nothing (MinorAmount 6001) (CurrencyCode "USD") bob day ExactSplit [RecordSplitRequest alice (MinorAmount 3000), RecordSplitRequest bob (MinorAmount 3001)] Nothing),
                  ("transfer", toJSON $ TransferRecordRequest (MinorAmount 2001) (CurrencyCode "USD") bob alice day Nothing)
                ]
          group <- runClient env $ cCreateGroup tc "Record notes"
          let gid@(GroupId rawGroup) = grpId group
              groupPath = "/groups/" <> show rawGroup
              send auth method path body = do
                response <- identityRequest port manager (Just auth) method path body
                assertEqual "record mutation status" status200 (HC.responseStatus response)
                case eitherDecode (HC.responseBody response) of
                  Left message -> assertFailure message >> fail message
                  Right saved -> do
                    assertWireNote (recordNoteOf saved) (HC.responseBody response)
                    pure saved
              checkRead expected = do
                let rid@(RecordId rawRecord) = extractRecordId expected
                response <- identityRequest port manager (Just token) "GET" (groupPath <> "/records/" <> show rawRecord) Null
                assertEqual "detail status" status200 (HC.responseStatus response)
                assertWireNote (recordNoteOf expected) (HC.responseBody response)
                assertEqual "detail preserves saved fields" (Right $ canonicalRecord expected) (canonicalRecord <$> eitherDecode (HC.responseBody response))
                public <- runClient env $ cGetRecords tc gid Nothing Nothing (Just 100) Nothing
                private <- runClient env $ allAdminRecords admin gid
                forM_ [pageItems public, private] $ \records ->
                  assertEqual "list preserves saved note and allocations" [canonicalRecord expected] [canonicalRecord record | record <- records, extractRecordId record == rid]
          _ <- runClient env $ cAddMember tc gid bob
          forM_ [(token, groupPath), (adminToken, "/admin" <> groupPath)] $ \(auth, prefix) ->
            forM_ cases $ \(kind, base) -> do
              original <- send auth "POST" (prefix <> "/records/" <> kind) (setField "note" (String exact) base)
              assertEqual "POST preserves exact Unicode/newlines/whitespace" (Just exact) (recordNoteOf original)
              checkRead original
              report <- runClient env $ cGetReport tc gid
              let RecordId rawRecord = extractRecordId original
              forM_ [(token, groupPath), (adminToken, "/admin" <> groupPath)] $ \(editor, editPrefix) -> do
                let path = editPrefix <> "/records/" <> kind <> "/" <> show rawRecord
                noted <- send editor "PUT" path (setField "note" (String changed) base)
                assertEqual "note-only PUT preserves saved allocations and money" (canonicalRecord $ withRecordNote (Just changed) original) (canonicalRecord noted)
                unchangedReport <- runClient env $ cGetReport tc gid
                assertEqual "note-only update preserves report and settlement" report unchangedReport
                checkRead noted
                dated <- send editor "PUT" path (setField "date" (String "2026-10-15") $ setField "note" (String changed) base)
                assertEqual "unrelated edit retains note" (Just changed) (recordNoteOf dated)
                assertEqual "unrelated date edit persisted" (read "2026-10-15") dated.date
                checkRead dated
                forM_ [omitNote base, setField "note" Null base, setField "note" (String " \t\n\x2003") base] $ \cleared -> do
                  saved <- send editor "PUT" path cleared
                  assertEqual "omitted, null and blank clear on replacement" Nothing (recordNoteOf saved)
                  checkRead saved
              forM_ [omitNote base, setField "note" Null base, setField "note" (String "\t\n ") base] $ \blank -> do
                saved <- send auth "POST" (prefix <> "/records/" <> kind) blank
                assertEqual "missing/null/blank accepted on create" Nothing (recordNoteOf saved)
                checkRead saved
              before <- sort <$> runClient env (allAdminRecords admin gid)
              beforeReport <- runClient env $ cGetReport tc gid
              forM_ [(setField "note" (Number 42) base, Nothing), (setField "unexpected" Null base, Nothing), (setField "note" (String "before\0after") base, Just "Note cannot contain NUL")] $ \(invalid, expectedMessage) ->
                forM_ [("POST", prefix <> "/records/" <> kind), ("PUT", prefix <> "/records/" <> kind <> "/" <> show rawRecord)] $ \(method, path) -> do
                  rejected <- identityRequest port manager (Just auth) method path invalid
                  assertEqual "invalid note/unknown field rejected" status400 (HC.responseStatus rejected)
                  forM_ expectedMessage $ \message -> assertEqual "NUL validation message" message (HC.responseBody rejected)
                  recordsAfter <- sort <$> runClient env (allAdminRecords admin gid)
                  assertEqual "rejected mutations leave records untouched" before recordsAfter
                  afterReport <- runClient env $ cGetReport tc gid
                  assertEqual "rejected notes leave accounting untouched" beforeReport afterReport,
      testCase "receipt own note and sibling notes remain independent across public/admin replacements" $
        withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env _ _ _ -> do
          token <- setupUser env "alice"
          alice <- fixtureUserId env "alice"
          adminToken <- setupAdmin env "admin" "adminpass123"
          let tc = mkTC token
              admin = mkAdminTC adminToken
              child :: Maybe Text -> ExpenseRecordRequest
              child value = ExpenseRecordRequest "Receipt child" value (MinorAmount 1001) (CurrencyCode "USD") alice (read "2026-10-14") ExactSplit [RecordSplitRequest alice (MinorAmount 1001)] Nothing
              first = "Invoice 42\n会议 <b>plain</b>"
          group <- runClient env $ cCreateGroup tc "Receipt notes"
          let gid = grpId group
              check receipt = do
                fetched <- runClient env $ cGetReceipt tc gid (receiptId receipt)
                assertEqual "receipt detail roundtrip" (canonicalReceipt receipt) (canonicalReceipt fetched)
                assertEqual "receipt own note retained" "Receipt envelope" (receiptNote fetched)
                assertEqual "child notes do not leak into siblings or inherit receipt note" [Nothing, Just first] (sort $ map recordNoteOf $ receiptRecords fetched)
                listed <- runClient env $ cAdminGroupReceipts admin gid
                assertEqual "admin receipt preserves child notes" [canonicalReceipt receipt] [canonicalReceipt item | item <- listed, receiptId item == receiptId receipt]
          forM_ [cCreateReceipt tc gid (CreateReceiptRequest "Receipt envelope" [child (Just first), child Nothing]), cAdminCreateReceipt admin gid (AdminCreateReceiptRequest alice "Receipt envelope" [child (Just first), child Nothing])] $ \create -> do
            receipt <- runClient env create
            check receipt
            forM_ [cUpdateReceipt tc, cAdminUpdateReceipt admin] $ \replace -> do
              updated <- runClient env $ replace gid (receiptId receipt) (UpdateReceiptRequest "Receipt envelope" [child Nothing, child (Just first)])
              check updated
              report <- runClient env $ cGetReport tc gid
              runClientExpectStatus env status400 $ replace gid (receiptId receipt) (UpdateReceiptRequest "Must not persist" [child (Just first), child (Just "\0")])
              restored <- runClient env $ cGetReceipt tc gid (receiptId receipt)
              assertEqual "invalid second child preserves receipt/child IDs and notes" (canonicalReceipt updated) (canonicalReceipt restored)
              afterReport <- runClient env $ cGetReport tc gid
              assertEqual "invalid child note preserves accounting" report afterReport,
      testCase "additive schema upgrade preserves records/allocations/reports and runs twice safely" $
        withTestAppServer Nothing id $ \env _ _ conn -> do
          token <- setupUser env "alice"
          alice <- fixtureUserId env "alice"
          _ <- setupUser env "bob"
          bob <- fixtureUserId env "bob"
          let tc = mkTC token
              day = read "2026-10-14"
              request = ExpenseRecordRequest "Old expense" Nothing (MinorAmount 1001) (CurrencyCode "USD") alice day ExactSplit [RecordSplitRequest alice (MinorAmount 333), RecordSplitRequest bob (MinorAmount 668)] Nothing
          group <- runClient env $ cCreateGroup tc "Upgrade notes"
          let gid = grpId group
          _ <- runClient env $ cAddMember tc gid bob
          oldExpense <- runClient env $ cAddExpense tc gid request
          oldIncome <- runClient env $ cAddIncome tc gid (IncomeRecordRequest "Old income" Nothing (MinorAmount 1001) (CurrencyCode "USD") bob day ExactSplit [RecordSplitRequest alice (MinorAmount 333), RecordSplitRequest bob (MinorAmount 668)] Nothing)
          oldTransfer <- runClient env $ cAddTransfer tc gid (TransferRecordRequest (MinorAmount 501) (CurrencyCode "USD") bob alice day Nothing)
          report <- runClient env $ cGetReport tc gid
          _ <- execute_ conn "ALTER TABLE records DROP COLUMN note"
          createTables conn
          createTables conn
          forM_ [oldExpense, oldIncome, oldTransfer] $ \original -> do
            saved <- runClient env $ cGetRecord tc gid (extractRecordId original)
            assertEqual "live API reads unchanged pre-upgrade record with null note" (canonicalRecord original) (canonicalRecord saved)
            assertEqual "old record note is null" Nothing (recordNoteOf saved)
          afterReport <- runClient env $ cGetReport tc gid
          assertEqual "upgrade preserves exact accounting" report afterReport
          let text = "Invoice 42\n会议 <b>plain</b>"
          edited <- runClient env $ cUpdateExpense tc gid (extractRecordId oldExpense) (request {ExpenseRequest.note = Just text})
          saved <- runClient env $ cGetRecord tc gid (extractRecordId edited)
          assertEqual "live upgraded API persists note without reallocating money" (canonicalRecord $ withRecordNote (Just text) oldExpense) (canonicalRecord saved)
    ]
  where
    setField key value (Object fields) = Object (KM.insert key value fields)
    setField _ _ _ = error "Expected object fixture"
    omitNote (Object fields) = Object (KM.delete "note" fields)
    omitNote _ = error "Expected object fixture"
    assertWireNote expected bytes = case eitherDecode bytes of
      Right (Object fields) -> assertEqual "response requires string/null note" (Just $ toJSON expected) (KM.lookup "note" fields)
      _ -> assertFailure "Expected record JSON object"
    canonicalReceipt (Receipt rid gid uploader note children created) =
      Receipt rid gid uploader note (sort $ map canonicalRecord children) created
    withRecordNote :: Maybe Text -> Record -> Record
    withRecordNote value record = record {StoredRecord.note = value}

recordQueryApiTests :: TestTree
recordQueryApiTests =
  testGroup
    "RecordQuery API"
    [ testCase "shared whole-ledger filtering, deterministic sorting, pagination, identities and scoped detail" $
        withTestAppServer (Just $ AdminBootstrap "admin" "adminpass123") id $ \env port manager conn -> do
          aliceToken <- setupUser env "alice"
          bobToken <- setupUser env "bob"
          outsiderToken <- setupUser env "outsider"
          adminToken <- setupAdmin env "admin" "adminpass123"
          aliceId <- fixtureUserId env "alice"
          bobId <- fixtureUserId env "bob"
          let tc = mkTC aliceToken
              bob = mkTC bobToken
              admin = mkAdminTC adminToken
          group <- runClient env $ cAdminCreateGroup admin (AdminCreateGroupRequest "Query ledger" aliceId MultiCurrency (CurrencyCode "USD"))
          let gid@(GroupId rawGroup) = grpId group
              splitRows = [RecordSplitRequest aliceId (MinorAmount 1), RecordSplitRequest bobId (MinorAmount 1)]
          _ <- runClient env $ cAddMember tc gid bobId
          transfer <- runClient env $ cAddTransfer tc gid (TransferRecordRequest (MinorAmount 2000) (CurrencyCode "USD") bobId aliceId (read "2026-10-04") (Just "needle Invoice 42"))
          income <- runClient env $ cAddIncome tc gid (IncomeRecordRequest "Bonus" (Just "needle") (MinorAmount 6000) (CurrencyCode "USD") bobId (read "2026-10-03") EqualSplit splitRows Nothing)
          expense <- runClient env $ cAddExpense tc gid (ExpenseRecordRequest "Team lunch" (Just "needle Invoice 42") (MinorAmount 9000) (CurrencyCode "USD") aliceId (read "2026-10-02") EqualSplit splitRows Nothing)
          let rid = extractRecordId
              e = rid expense
              i = rid income
              t = rid transfer
          forM_ [(transfer, "2026-10-10 00:00:00 UTC"), (income, "2026-10-11 00:00:00 UTC"), (expense, "2026-10-12 00:00:00 UTC")] $ \(record, timestamp) -> do
            let RecordId raw = rid record
            _ <- execute conn "UPDATE records SET created_at = ? WHERE id = ?" (read timestamp :: UTCTime, raw)
            pure ()
          forM_ [1 .. 25 :: Int] $ \_ -> void $ runClient env $ cAddExpense tc gid (usdExpense "Filler" 1 aliceId (read "2026-10-20") splitRows)
          _ <- execute conn "UPDATE records SET created_at = ? WHERE group__id = ? AND title = 'Filler'" (read "2026-10-20 00:00:00 UTC" :: UTCTime, rawGroup)
          let endpoints = [cGetRecords tc gid, cAdminGroupRecords admin gid]
              check queryText wanted = forM_ endpoints $ \endpoint -> do
                page <- runClient env $ endpoint (Just queryText) Nothing (Just 100) Nothing
                assertEqual (T.unpack queryText) wanted (map rid $ pageItems page)
                let PageInfo _ _ count _ _ = pageMeta page
                assertEqual "total describes filtered complete ledger" (length wanted) count
          forM_ endpoints $ \endpoint -> do
            first <- runClient env $ endpoint Nothing Nothing Nothing Nothing
            assertEqual "default sort is observable" (Just "-date") (appliedSort first)
            assertBool "only match is beyond former first twenty" (e `notElem` map rid (pageItems first))
            page <- runClient env $ endpoint (Just "type:expense payer:@alice date:2026-10-01..2026-10-31 currency:USD amount:>=25 title:\"team lunch\" note:\"invoice 42\"") Nothing (Just 1) Nothing
            assertEqual "filters precede pagination" [e] (map rid $ pageItems page)
            assertEqual "filtered metadata" (PageInfo 0 1 1 False False) (pageMeta page)
            forM_ [("date", [e, i, t]), ("-date", [t, i, e]), ("createdAt", [t, i, e]), ("-createdAt", [e, i, t]), ("title", [i, e, t]), ("-title", [t, e, i]), ("type", [e, i, t]), ("-type", [t, i, e]), ("payer", [e, t, i]), ("-payer", [t, e, i]), ("payee", [t, i, e]), ("-payee", [i, t, e]), ("amount", [t, i, e]), ("-amount", [e, i, t]), ("currency", sort [e, i, t]), ("-currency", sort [e, i, t])] $ \(ordering, wanted) -> do
              sorted <- runClient env $ endpoint (Just "note:needle") Nothing (Just 100) (Just ordering)
              assertEqual ("typed sort " ++ T.unpack ordering) wanted (map rid $ pageItems sorted)
            tied <- runClient env $ endpoint (Just "title:Filler") Nothing (Just 100) (Just "-date")
            let expectedTies = sort $ map rid $ pageItems tied
            assertEqual "equal dates retain UUID order descending" expectedTies (map rid $ pageItems tied)
            forM_ (zip [0 ..] expectedTies) $ \(offset, expectedId) -> do
              tiedPage <- runClient env $ endpoint (Just "title:Filler") (Just offset) (Just 1) (Just "-date")
              assertEqual "ties remain stable across pages" [expectedId] (map rid $ pageItems tiedPage)
            clamped <- runClient env $ endpoint (Just "note:needle") (Just (-1)) (Just 1000) Nothing
            assertEqual "existing offset and limit clamp" (PageInfo 0 100 3 False False) (pageMeta clamped)
          check "type:income payee:@bob" [i]
          check "payer:@bob payee:@alice" [t]
          check "type:income payer:@bob" []
          check "type:expense payee:@bob" []
          check "type:expense member:@bob note:needle" [e]
          check "member:unknown.valid" []
          check "member:00000000-0000-0000-0000-000000000000" []
          check "alice@example.test" []
          check "created:2026-10-12" [e]
          check "date:2026-10-02" [e]
          historical <- runClient env $ cAdminGroupRecord admin gid e
          assertEqual "detail is independent of list page" e (rid historical)
          _ <- runClient env $ cDeleteMember tc gid bobId
          _ <- runClient env $ cUpdateMe bob (UpdateProfileRequest "robert" "Former Member")
          check "note:needle member:@bob" []
          check "type:income payee:@robert" [i]
          check "type:income member:\"former member\"" [i]
          let UserId bobUuid = bobId
          check ("type:income member:" <> UUID.toText bobUuid) [i]
          report <- runClient env $ cGetReport tc gid
          forM_ endpoints $ \endpoint -> do
            forM_ ["nope:value", "title:\"", "date:2026-02-29", "amount:5", "currency:USD currency:JPY amount:5", "currency:JPY amount:5.1", "currency:USD amount:92233720368547758.08"] $ \queryText -> do
              result <- runClientM (endpoint (Just queryText) Nothing Nothing Nothing) env
              case result of
                Left (FailureResponse _ response) -> do
                  assertEqual "invalid query status" status400 (responseStatusCode response)
                  assertBool "specific query error prefix" ("Invalid record query: " `LBS8.isPrefixOf` responseBody response)
                other -> assertFailure $ "Expected record query rejection, got " ++ show other
            forM_ ["", "unknown", "-", "Date"] $ \ordering -> do
              result <- runClientM (endpoint Nothing Nothing Nothing (Just ordering)) env
              case result of
                Left (FailureResponse _ response) -> do
                  assertEqual "invalid sort status" status400 (responseStatusCode response)
                  assertEqual "invalid sort message" "Invalid record sort" (responseBody response)
                other -> assertFailure $ "Expected sort rejection, got " ++ show other
          unchangedReport <- runClient env $ cGetReport tc gid
          assertEqual "bad queries cannot mutate finances" report unchangedReport
          allRecords <- runClient env $ allAdminRecords admin gid
          assertEqual "all-page assertion inspects entire ledger" 28 (length allRecords)
          _ <- runClient env $ cAddMember tc gid bobId
          yen <- runClient env $ cAdminAddTransfer admin gid (TransferRecordRequest (MinorAmount 100000) (CurrencyCode "JPY") bobId aliceId (read "2026-10-05") (Just "needle"))
          forM_ endpoints $ \endpoint -> do
            forM_ [("amount", [rid yen, t, i, e]), ("-amount", [rid yen, e, i, t])] $ \(ordering, wanted) -> do
              page <- runClient env $ endpoint (Just "note:needle") Nothing (Just 100) (Just ordering)
              assertEqual "amount keeps currency ascending even when descending" wanted (map rid $ pageItems page)
          foreignGroup <- runClient env $ cCreateGroup tc "Another ledger"
          runClientExpectStatus env status404 $ cAdminGroupRecord admin (grpId foreignGroup) e
          runClientExpectStatus env status404 $ cAdminGroupRecord admin gid (RecordId UUID.nil)
          runClientExpectStatus env status404 $ cAdminGroupRecord admin (GroupId UUID.nil) e
          runClientExpectStatus env status404 $ cAdminGroupRecords admin (GroupId UUID.nil) Nothing Nothing Nothing Nothing
          runClientExpectStatus env status403 $ cGetRecords (mkTC outsiderToken) gid (Just "nope:bad") Nothing Nothing Nothing
          runClientExpectStatus env status401 $ cAdminGroupRecords (mkAdminTC aliceToken) gid Nothing Nothing Nothing Nothing
          runClientExpectStatus env status401 $ cAdminGroupRecord (mkAdminTC aliceToken) gid e
          forM_ ["/groups/", "/admin/groups/"] $ \prefix -> do
            response <- identityRequest port manager Nothing "GET" (prefix <> show rawGroup <> "/records?query=nope:bad") Null
            assertEqual "unauthenticated query never bypasses authorization" status401 (HC.responseStatus response)
    ]
