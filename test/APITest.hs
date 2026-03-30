{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module APITest (apiTests) where

import Data.ByteString.Char8 (pack)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Jizhang.API (app)
import Jizhang.API.Admin (AdminAPI)
import Jizhang.API.AdminAuth (AdminAuthAPI)
import Jizhang.API.Auth (AuthAPI)
import Jizhang.API.Group (GroupAPI)
import Jizhang.API.Import (ImportAPI)
import Jizhang.API.Receipt (ReceiptAPI)
import Jizhang.API.Record (RecordAPI)
import Jizhang.API.Report (ReportAPI)
import Jizhang.API.Types
import Jizhang.API.User (UserAPI)
import Jizhang.Database.Init (createTables, dropTables)
import Log (runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp (testWithApplication)
import Servant hiding (addHeader)
import Servant.Auth (Auth, JWT)
import Servant.Auth.Client (Token (..))
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import Servant.Client
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

-- ============================================================
-- Servant client functions
-- ============================================================

-- Unprotected auth endpoints
register_ :: RegisterRequest -> ClientM User
login_ :: LoginRequest -> ClientM LoginResponse
(register_ :<|> login_) = client (Proxy :: Proxy AuthAPI)

adminLogin_ :: AdminLoginRequest -> ClientM AdminLoginResponse
adminLogin_ = client (Proxy :: Proxy AdminAuthAPI)

-- Protected endpoints (require JWT auth token)
type TestProtectedAPI =
  Auth '[JWT] AuthUser :> (UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ImportAPI :<|> ReceiptAPI)

type TestProtectedAdminAPI =
  Auth '[JWT] AuthAdmin :> AdminAPI

protectedClient_ ::
  Token ->
  Client ClientM (UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ImportAPI :<|> ReceiptAPI)
protectedClient_ = client (Proxy :: Proxy TestProtectedAPI)

protectedAdminClient_ :: Token -> Client ClientM AdminAPI
protectedAdminClient_ = client (Proxy :: Proxy TestProtectedAdminAPI)

-- Convenience record holding all destructured client functions
data TC = TC
  { cGetMe :: ClientM User,
    cDeleteMe :: ClientM NoContent,
    cGetMyGroups :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Group),
    cCreateGroup :: Text -> ClientM Group,
    cGetGroupById :: GroupId -> ClientM Group,
    cUpdateGroupById :: GroupId -> Text -> ClientM Group,
    cDeleteGroupById :: GroupId -> ClientM NoContent,
    cAddMember :: GroupId -> Username -> ClientM Group,
    cDeleteMember :: GroupId -> Username -> ClientM NoContent,
    cTransferOwnership :: GroupId -> Username -> ClientM Group,
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
    cUpdateReceipt :: GroupId -> ReceiptId -> UpdateReceiptRequest -> ClientM Receipt
  }

data AdminTC = AdminTC
  { cAdminUsers :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse User),
    cAdminCreateUser :: AdminCreateUserRequest -> ClientM User,
    cAdminBulkDeleteUsers :: BulkRequest UserId -> ClientM NoContent,
    cAdminDeleteUser :: UserId -> ClientM NoContent,
    cAdminAdmins :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse AdminSummary),
    cAdminCreateAdmin :: AdminCreateAdminRequest -> ClientM AdminSummary,
    cAdminGroups :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> ClientM (PaginatedResponse Group),
    cAdminBulkDeleteGroups :: BulkRequest GroupId -> ClientM NoContent,
    cAdminGroup :: GroupId -> ClientM Group,
    cAdminUpdateGroup :: GroupId -> Text -> ClientM Group,
    cAdminDeleteGroup :: GroupId -> ClientM NoContent,
    cAdminAddMember :: GroupId -> Username -> ClientM Group,
    cAdminBulkAddMembers :: GroupId -> BulkRequest Username -> ClientM Group,
    cAdminDeleteMember :: GroupId -> Username -> ClientM NoContent,
    cAdminBulkDeleteMembers :: GroupId -> BulkRequest Username -> ClientM NoContent,
    cAdminTransferOwnership :: GroupId -> Username -> ClientM Group,
    cAdminGroupRecords :: GroupId -> ClientM [Record],
    cAdminBulkDeleteRecords :: GroupId -> BulkRequest RecordId -> ClientM NoContent,
    cAdminDeleteRecord :: GroupId -> RecordId -> ClientM NoContent,
    cAdminUpdateTransfer :: GroupId -> RecordId -> TransferRecordRequest -> ClientM Record,
    cAdminUpdateExpense :: GroupId -> RecordId -> ExpenseRecordRequest -> ClientM Record,
    cAdminGroupReport :: GroupId -> ClientM Report,
    cAdminGroupReceipts :: GroupId -> ClientM [Receipt],
    cAdminBulkDeleteReceipts :: GroupId -> BulkRequest ReceiptId -> ClientM NoContent,
    cAdminDeleteReceipt :: GroupId -> ReceiptId -> ClientM NoContent,
    cAdminUpdateReceipt :: GroupId -> ReceiptId -> UpdateReceiptRequest -> ClientM Receipt
  }

mkTC :: Text -> TC
mkTC tok =
  let auth = Token (encodeUtf8 tok)
      (uc :<|> gc :<|> rc :<|> rpt :<|> _imp :<|> rcp) = protectedClient_ auth
      (a1 :<|> a2 :<|> a3) = uc
      (b1 :<|> b2 :<|> b3 :<|> b4 :<|> b5 :<|> b6 :<|> b7) = gc
      (c1 :<|> c2 :<|> c3 :<|> c4 :<|> c5 :<|> c6 :<|> c7) = rc
      (d1 :<|> d2 :<|> d3 :<|> d4 :<|> d5) = rcp
   in TC a1 a2 a3 b1 b2 b3 b4 b5 b6 b7 c1 c2 c3 c4 c5 c6 c7 rpt d1 d2 d3 d4 d5

mkAdminTC :: Text -> AdminTC
mkAdminTC tok =
  let auth = Token (encodeUtf8 tok)
      (a1 :<|> a2 :<|> a3 :<|> a4 :<|> a5 :<|> a6 :<|> a7 :<|> a8 :<|> a9 :<|> a10 :<|> a11 :<|> a12 :<|> a13 :<|> a14 :<|> a15 :<|> a16 :<|> a17 :<|> a18 :<|> a19 :<|> a20 :<|> a21 :<|> a22 :<|> a23 :<|> a24 :<|> a25 :<|> a26) = protectedAdminClient_ auth
   in AdminTC a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26

-- ============================================================
-- Test infrastructure
-- ============================================================

withTestApp :: (ClientEnv -> IO a) -> IO a
withTestApp = withTestAppWithBootstrap Nothing

withTestAppWithBootstrap :: Maybe AdminBootstrap -> (ClientEnv -> IO a) -> IO a
withTestAppWithBootstrap adminBootstrap action = withStdOutLogger $ \logger -> do
  connStr <- fromMaybe "dbname=jizhang_test" . fmap pack <$> lookupEnv "TEST_DATABASE_URL"
  conn <- connectPostgreSQL connStr
  dropTables conn
  createTables conn
  jwk <- generateKey
  let jwtCfg = defaultJWTSettings jwk
      appEnv = AppEnv conn jwtCfg adminBootstrap
  waiApp <- runLogT "test" logger maxBound $ app appEnv
  manager <- newManager defaultManagerSettings
  testWithApplication (pure waiApp) $ \port -> do
    let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
    result <- action env
    dropTables conn
    close conn
    pure result

-- | Run a ClientM action, fail the test if it returns Left
runClient :: ClientEnv -> ClientM a -> IO a
runClient env action = do
  result <- runClientM action env
  case result of
    Left err -> assertFailure ("Client error: " ++ show err) >> undefined
    Right a -> pure a

-- | Run a ClientM action, expect it to fail with a specific status code
runClientExpectStatus :: ClientEnv -> Status -> ClientM a -> IO ()
runClientExpectStatus env expectedStatus action = do
  result <- runClientM action env
  case result of
    Left (FailureResponse _ resp) ->
      assertEqual "status code" expectedStatus (responseStatusCode resp)
    Left err -> assertFailure ("Expected FailureResponse but got: " ++ show err)
    Right _ -> assertFailure "Expected failure but got success"

-- | Assert that a Double is approximately equal
assertApproxEqual :: String -> Double -> Double -> Assertion
assertApproxEqual msg expected actual =
  assertBool
    (msg ++ ": expected " ++ show expected ++ " but got " ++ show actual)
    (abs (expected - actual) < 1e-9)

-- | Register a user and log in, return session token
setupUser :: ClientEnv -> Text -> IO Text
setupUser env name = do
  _ <- runClient env $ register_ (RegisterRequest name "password123")
  resp <- runClient env $ login_ (LoginRequest name "password123")
  pure resp.accessToken

loginWithPassword :: ClientEnv -> Text -> Text -> IO Text
loginWithPassword env name password = do
  resp <- runClient env $ login_ (LoginRequest name password)
  pure resp.accessToken

setupAdmin :: ClientEnv -> Text -> Text -> IO Text
setupAdmin env name password = do
  resp <- runClient env $ adminLogin_ (AdminLoginRequest name password)
  pure resp.accessToken

-- ============================================================
-- Helpers
-- ============================================================

-- | Extract username text from a User (avoids DuplicateRecordFields ambiguity)
userName :: User -> Text
userName (User _ n) = n

grpId :: Group -> GroupId
grpId (Group gid _ _ _) = gid

grpName :: Group -> Text
grpName (Group _ n _ _) = n

grpOwnerName :: Group -> Text
grpOwnerName (Group _ _ o _) = userName o

grpMemberNames :: Group -> [Text]
grpMemberNames (Group _ _ _ ms) = sort $ map userName ms

titleOfRecord :: Record -> Text
titleOfRecord ExpenseRecord {title = t} = t
titleOfRecord TransferRecord {title = t} = t

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

extractRecordId :: Record -> RecordId
extractRecordId ExpenseRecord {..} = recordId
extractRecordId TransferRecord {..} = recordId

receiptNote :: Receipt -> Text
receiptNote (Receipt _ _ _ n _ _) = n

receiptRecords :: Receipt -> [Record]
receiptRecords (Receipt _ _ _ _ rs _) = rs

-- ============================================================
-- Tests
-- ============================================================

apiTests :: TestTree
apiTests =
  testGroup
    "API Integration"
    [ authTests,
      adminTests,
      userTests,
      groupTests,
      recordTests,
      reportIntegrationTests,
      authorizationTests,
      receiptTests
    ]

-- Auth tests
authTests :: TestTree
authTests =
  testGroup
    "Auth"
    [ testCase "register and login" $ withTestApp $ \env -> do
        u <- runClient env $ register_ (RegisterRequest "alice" "password123")
        assertEqual "username" "alice" (userName u)
        resp <- runClient env $ login_ (LoginRequest "alice" "password123")
        assertEqual "login user" "alice" (userName $ loginUser resp)
        assertBool "access token not empty" (not $ T.null resp.accessToken),
      --
      testCase "register duplicate => 409" $ withTestApp $ \env -> do
        _ <- runClient env $ register_ (RegisterRequest "alice" "password123")
        runClientExpectStatus env status409 $ register_ (RegisterRequest "alice" "password123"),
      --
      testCase "login wrong password => 401" $ withTestApp $ \env -> do
        _ <- runClient env $ register_ (RegisterRequest "alice" "password123")
        runClientExpectStatus env status401 $ login_ (LoginRequest "alice" "wrongpassword"),
      --
      testCase "login nonexistent user => 401" $ withTestApp $ \env -> do
        runClientExpectStatus env status401 $ login_ (LoginRequest "nobody" "password123"),
      --
      testCase "protected endpoint without token => 401" $ withTestApp $ \env -> do
        let tc = mkTC "invalid-token"
        runClientExpectStatus env status401 (cGetMe tc)
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
        assertEqual "usernames" ["bob"] (sort $ map userName $ pageItems users),
      testCase "app user token cannot access admin endpoints => 401" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokBob <- setupUser env "bob"
        runClientExpectStatus env status401 $ cAdminUsers (mkAdminTC tokBob) Nothing Nothing Nothing Nothing,
      testCase "admin can create users and admins" $ withTestAppWithBootstrap (Just $ AdminBootstrap "rootadmin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "rootadmin" "adminpass123"
        let tcAdmin = mkAdminTC tokAdmin
        createdUser <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "carol" "password123")
        assertEqual "created user" "carol" (userName createdUser)
        createdAdmin <- runClient env $ cAdminCreateAdmin tcAdmin (AdminCreateAdminRequest "ops" "password123")
        assertEqual "created admin" "ops" (adminSummaryUsername createdAdmin)
        tokOps <- setupAdmin env "ops" "password123"
        let tcOps = mkAdminTC tokOps
        admins <- runClient env $ cAdminAdmins tcOps Nothing Nothing Nothing Nothing
        assertEqual "admin usernames" ["ops", "rootadmin"] (sort $ map adminSummaryUsername $ pageItems admins),
      testCase "admin list endpoints support query filters" $ withTestAppWithBootstrap (Just $ AdminBootstrap "rootadmin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "rootadmin" "adminpass123"
        let tcAdmin = mkAdminTC tokAdmin
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "alice" "password123")
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "bob" "password123")
        _ <- runClient env $ cAdminCreateAdmin tcAdmin (AdminCreateAdminRequest "ops-team" "password123")
        tokAlice <- loginWithPassword env "alice" "password123"
        let tcAlice = mkTC tokAlice
        _ <- runClient env $ cCreateGroup tcAlice "Summer Trip"
        _ <- runClient env $ cCreateGroup tcAlice "Work Budget"
        users <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationQuery = Just "ali"}
        assertEqual "filtered users" ["alice"] (map userName $ pageItems users)
        admins <- runClient env $ withPagination (cAdminAdmins tcAdmin) defaultPaginationParams {paginationQuery = Just "ops"}
        assertEqual "filtered admins" ["ops-team"] (map adminSummaryUsername $ pageItems admins)
        groups <- runClient env $ withPagination (cAdminGroups tcAdmin) defaultPaginationParams {paginationQuery = Just "trip"}
        assertEqual "filtered groups" ["Summer Trip"] (map grpName $ pageItems groups),
      testCase "admin list endpoints support pagination and sorting" $ withTestAppWithBootstrap (Just $ AdminBootstrap "rootadmin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "rootadmin" "adminpass123"
        let tcAdmin = mkAdminTC tokAdmin
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "charlie" "password123")
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "alice" "password123")
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "bob" "password123")
        page1 <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationOffset = Just 0, paginationLimit = Just 2, paginationSort = Just "username"}
        let PageInfo off1 lim1 total1 hasNext1 hasPrev1 = pageMeta page1
        assertEqual "page1 users" ["alice", "bob"] (map userName $ pageItems page1)
        assertEqual "page1 offset" 0 off1
        assertEqual "page1 limit" 2 lim1
        assertEqual "page1 total" 3 total1
        assertEqual "page1 hasNext" True hasNext1
        assertEqual "page1 hasPrev" False hasPrev1
        page2 <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationOffset = Just 2, paginationLimit = Just 2, paginationSort = Just "username"}
        assertEqual "page2 users" ["charlie"] (map userName $ pageItems page2)
        descPage <- runClient env $ withPagination (cAdminUsers tcAdmin) defaultPaginationParams {paginationOffset = Just 0, paginationLimit = Just 3, paginationSort = Just "-username"}
        assertEqual "descending users" ["charlie", "bob", "alice"] (map userName $ pageItems descPage),
      testCase "admin can inspect groups without membership" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        _ <- runClient env $ cAddExpense tcAlice (grpId g) (ExpenseRecordRequest "Dinner" 100.0 (Username "alice") (read "2025-06-01") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1])
        groups <- runClient env $ cAdminGroups tcAdmin Nothing Nothing Nothing Nothing
        assertEqual "group names" ["Trip"] (map grpName $ pageItems groups)
        fetched <- runClient env $ cAdminGroup tcAdmin (grpId g)
        assertEqual "fetched group name" "Trip" (grpName fetched)
        records <- runClient env $ cAdminGroupRecords tcAdmin (grpId g)
        assertEqual "record count" 1 (length records)
        report <- runClient env $ cAdminGroupReport tcAdmin (grpId g)
        assertEqual "settlement count" 1 (length $ settlements report)
        receipts <- runClient env $ cAdminGroupReceipts tcAdmin (grpId g)
        assertEqual "receipt count" 0 (length receipts),
      testCase "admin can update group and membership without ownership" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        g1 <- runClient env $ cAdminUpdateGroup tcAdmin (grpId g) "Renamed Trip"
        assertEqual "updated name" "Renamed Trip" (grpName g1)
        g2 <- runClient env $ cAdminAddMember tcAdmin (grpId g) (Username "bob")
        assertEqual "members after add" ["alice", "bob"] (sort $ grpMemberNames g2)
        _ <- runClient env $ cAdminTransferOwnership tcAdmin (grpId g) (Username "bob")
        fetched <- runClient env $ cAdminGroup tcAdmin (grpId g)
        assertEqual "new owner" "bob" (grpOwnerName fetched)
        _ <- runClient env $ cAdminDeleteMember tcAdmin (grpId g) (Username "alice")
        fetched2 <- runClient env $ cAdminGroup tcAdmin (grpId g)
        assertEqual "members after delete" ["bob"] (grpMemberNames fetched2),
      testCase "admin can update and delete records" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        expense <- runClient env $ cAddExpense tcAlice (grpId g) (ExpenseRecordRequest "Dinner" 100.0 (Username "alice") (read "2025-06-01") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1])
        expense' <- runClient env $ cAdminUpdateExpense tcAdmin (grpId g) (extractRecordId expense) (ExpenseRecordRequest "Lunch" 40.0 (Username "bob") (read "2025-06-02") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1])
        case expense' of
          ExpenseRecord {title = recordTitle, amount = recordAmount, paidBy = payer} -> do
            assertEqual "updated title" "Lunch" recordTitle
            assertEqual "updated amount" 40.0 recordAmount
            assertEqual "updated payer" "bob" (userName payer)
          _ -> assertFailure "Expected updated expense record"
        transfer <- runClient env $ cAddTransfer tcAlice (grpId g) (TransferRecordRequest 10.0 (Username "alice") (Username "bob") (read "2025-06-03"))
        transfer' <- runClient env $ cAdminUpdateTransfer tcAdmin (grpId g) (extractRecordId transfer) (TransferRecordRequest 25.0 (Username "bob") (Username "alice") (read "2025-06-04"))
        case transfer' of
          TransferRecord {amount = transferAmount, paidBy = payer, transferTo = receiver} -> do
            assertEqual "updated transfer amount" 25.0 transferAmount
            assertEqual "updated transfer payer" "bob" (userName payer)
            assertEqual "updated transfer receiver" "alice" (userName receiver)
          _ -> assertFailure "Expected updated transfer record"
        _ <- runClient env $ cAdminDeleteRecord tcAdmin (grpId g) (extractRecordId transfer)
        runClientExpectStatus env status404 $ cGetRecord tcAlice (grpId g) (extractRecordId transfer),
      testCase "admin can update and delete receipts" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAdmin = mkAdminTC tokAdmin
            tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        receipt <- runClient env $ cCreateReceipt tcAlice (grpId g) (CreateReceiptRequest "Original" [ExpenseRecordRequest "Dinner" 100.0 (Username "alice") (read "2025-06-01") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]])
        receipt' <- runClient env $ cAdminUpdateReceipt tcAdmin (grpId g) (receiptId receipt) (UpdateReceiptRequest "Updated" [ExpenseRecordRequest "Taxi" 60.0 (Username "bob") (read "2025-06-02") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]])
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
        _ <- runClient env $ cAdminDeleteUser tcAdmin (userId alice)
        runClientExpectStatus env status404 $ cGetMe tcAlice,
      testCase "admin bulk operations work" $ withTestAppWithBootstrap (Just $ AdminBootstrap "admin" "adminpass123") $ \env -> do
        tokAdmin <- setupAdmin env "admin" "adminpass123"
        tokAlice <- setupUser env "alice"
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
        _ <- runClient env $ cAdminBulkDeleteUsers tcAdmin (BulkRequest [userId bobUser, userId carolUser])
        usersAfterDelete <- runClient env $ cAdminUsers tcAdmin Nothing Nothing Nothing (Just "username")
        assertEqual "users after bulk delete" ["alice"] (map userName $ pageItems usersAfterDelete)
        g1 <- runClient env $ cCreateGroup tcAlice "Trip"
        g2 <- runClient env $ cCreateGroup tcAlice "Work"
        g3 <- runClient env $ cCreateGroup tcAlice "Archive"
        _ <- runClient env $ cAdminBulkDeleteGroups tcAdmin (BulkRequest [grpId g2])
        _ <- runClient env $ cAdminDeleteGroup tcAdmin (grpId g3)
        groupsAfterDelete <- runClient env $ cAdminGroups tcAdmin Nothing Nothing Nothing (Just "groupName")
        assertEqual "remaining groups" ["Trip"] (map grpName $ pageItems groupsAfterDelete)
        _ <- runClient env $ cAdminCreateUser tcAdmin (AdminCreateUserRequest "dave" "password123")
        _ <- runClient env $ cAdminBulkAddMembers tcAdmin (grpId g1) (BulkRequest [Username "dave"])
        fetched1 <- runClient env $ cAdminGroup tcAdmin (grpId g1)
        assertEqual "members after bulk add" ["alice", "dave"] (grpMemberNames fetched1)
        _ <- runClient env $ cAdminBulkDeleteMembers tcAdmin (grpId g1) (BulkRequest [Username "dave"])
        fetched2 <- runClient env $ cAdminGroup tcAdmin (grpId g1)
        assertEqual "members after bulk delete" ["alice"] (grpMemberNames fetched2)
        _ <- runClient env $ cAdminBulkAddMembers tcAdmin (grpId g1) (BulkRequest [Username "dave"])
        expense <- runClient env $ cAddExpense tcAlice (grpId g1) (ExpenseRecordRequest "Dinner" 100.0 (Username "alice") (read "2025-06-01") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "dave") 1])
        transfer <- runClient env $ cAddTransfer tcAlice (grpId g1) (TransferRecordRequest 15.0 (Username "dave") (Username "alice") (read "2025-06-02"))
        _ <- runClient env $ cAdminBulkDeleteRecords tcAdmin (grpId g1) (BulkRequest [extractRecordId expense, extractRecordId transfer])
        recordsAfterDelete <- runClient env $ cAdminGroupRecords tcAdmin (grpId g1)
        assertEqual "records after bulk delete" 0 (length recordsAfterDelete)
        receipt <- runClient env $ cCreateReceipt tcAlice (grpId g1) (CreateReceiptRequest "Original" [ExpenseRecordRequest "Taxi" 60.0 (Username "alice") (read "2025-06-03") [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "dave") 1]])
        _ <- runClient env $ cAdminBulkDeleteReceipts tcAdmin (grpId g1) (BulkRequest [receiptId receipt])
        receiptsAfterDelete <- runClient env $ cAdminGroupReceipts tcAdmin (grpId g1)
        assertEqual "receipts after bulk delete" 0 (length receiptsAfterDelete)
    ]

-- User tests
userTests :: TestTree
userTests =
  testGroup
    "Users"
    [ testCase "get me" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        u <- runClient env $ cGetMe tc
        assertEqual "username" "alice" (userName u),
      --
      testCase "delete me" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        let tc = mkTC tokAlice
        _ <- runClient env $ cDeleteMe tc
        -- Token is now invalid (user gone), so /users/me should fail
        runClientExpectStatus env status404 $ cGetMe tc
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
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        let gid = grpId g
        g' <- runClient env $ cAddMember tc gid (Username "bob")
        assertEqual "has both" ["alice", "bob"] (grpMemberNames g')
        _ <- runClient env $ cDeleteMember tc gid (Username "bob")
        g'' <- runClient env $ cGetGroupById tc gid
        assertEqual "only owner after deactivation" ["alice"] (grpMemberNames g'')
        -- Re-adding reactivates
        g''' <- runClient env $ cAddMember tc gid (Username "bob")
        assertEqual "reactivated" ["alice", "bob"] (grpMemberNames g'''),
      --
      testCase "member can leave group" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let gid = grpId g
        _ <- runClient env $ cAddMember tcAlice gid (Username "bob")
        -- Bob leaves on his own
        _ <- runClient env $ cDeleteMember tcBob gid (Username "bob")
        g' <- runClient env $ cGetGroupById tcAlice gid
        assertEqual "bob left" ["alice"] (grpMemberNames g'),
      --
      testCase "cannot deactivate group owner" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        runClientExpectStatus env status400 $ cDeleteMember tc (grpId g) (Username "alice"),
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
    [ testCase "create expense record with splits" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req =
              ExpenseRecordRequest
                { title = "Dinner",
                  amount = 100.0,
                  byUsername = Username "alice",
                  date = read "2025-06-01",
                  splits =
                    [ RecordSplitRequest (Username "alice") 1,
                      RecordSplitRequest (Username "bob") 1
                    ]
                }
        r <- runClient env $ cAddExpense tc (grpId g) req
        case r of
          ExpenseRecord {..} -> do
            assertEqual "title" "Dinner" title
            assertEqual "splits count" 2 (length splits)
          _ -> assertFailure "Expected ExpenseRecord",
      --
      testCase "create transfer record" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req =
              TransferRecordRequest
                { amount = 50.0,
                  byUsername = Username "alice",
                  toUsername = Username "bob",
                  date = read "2025-06-01"
                }
        r <- runClient env $ cAddTransfer tc (grpId g) req
        case r of
          TransferRecord {..} -> do
            assertEqual "from" "alice" (userName paidBy)
            assertEqual "to" "bob" (userName transferTo)
          _ -> assertFailure "Expected TransferRecord",
      --
      testCase "delete record" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req =
              ExpenseRecordRequest
                { title = "Lunch",
                  amount = 30.0,
                  byUsername = Username "alice",
                  date = read "2025-06-01",
                  splits = [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                }
        r <- runClient env $ cAddExpense tc (grpId g) req
        _ <- runClient env $ cDeleteRecord tc (grpId g) (extractRecordId r)
        runClientExpectStatus env status404 $ cGetRecord tc (grpId g) (extractRecordId r),
      --
      testCase "list records in group" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req1 =
              ExpenseRecordRequest
                { title = "Lunch",
                  amount = 30.0,
                  byUsername = Username "alice",
                  date = read "2025-06-01",
                  splits = [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                }
            req2 =
              TransferRecordRequest
                { amount = 10.0,
                  byUsername = Username "bob",
                  toUsername = Username "alice",
                  date = read "2025-06-02"
                }
        _ <- runClient env $ cAddExpense tc (grpId g) req1
        _ <- runClient env $ cAddTransfer tc (grpId g) req2
        rs <- runClient env $ withPagination1 (cGetRecords tc) (grpId g) defaultPaginationParams
        assertEqual "two records" 2 (length $ pageItems rs),
      --
      testCase "transfer to self => 400" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        let req =
              TransferRecordRequest
                { amount = 10.0,
                  byUsername = Username "alice",
                  toUsername = Username "alice",
                  date = read "2025-06-01"
                }
        runClientExpectStatus env status400 $ cAddTransfer tc (grpId g) req,
      --
      testCase "zero amount => 400" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        let req =
              ExpenseRecordRequest
                { title = "Free",
                  amount = 0.0,
                  byUsername = Username "alice",
                  date = read "2025-06-01",
                  splits = [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                }
        runClientExpectStatus env status400 $ cAddExpense tc (grpId g) req
    ]

-- Report / settlement integration test
reportIntegrationTests :: TestTree
reportIntegrationTests =
  testGroup
    "Report"
    [ testCase "settlement of simple expenses" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        -- Alice pays $100, split evenly
        let req =
              ExpenseRecordRequest
                { title = "Hotel",
                  amount = 100.0,
                  byUsername = Username "alice",
                  date = read "2025-06-01",
                  splits =
                    [ RecordSplitRequest (Username "alice") 1,
                      RecordSplitRequest (Username "bob") 1
                    ]
                }
        _ <- runClient env $ cAddExpense tc (grpId g) req
        report <- runClient env $ cGetReport tc (grpId g)
        -- Bob owes Alice $50
        assertEqual "one settlement" 1 (length $ settlements report)
        let Settlement sFrom sTo sAmt = head $ settlements report
        assertEqual "from" "bob" (userName sFrom)
        assertEqual "to" "alice" (userName sTo)
        assertApproxEqual "amount" 50.0 sAmt
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
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        -- Bob (not owner) tries to add charlie
        runClientExpectStatus env status403 $ cAddMember tcBob (grpId g) (Username "charlie"),
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
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let req = TransferRecordRequest 10.0 (Username "alice") (Username "bob") (read "2025-06-01")
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
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        let req = TransferRecordRequest 10.0 (Username "alice") (Username "bob") (read "2025-06-01")
        r <- runClient env $ cAddTransfer tcBob (grpId g) req
        case r of
          TransferRecord {} -> pure ()
          _ -> assertFailure "Expected TransferRecord",
      --
      testCase "adding duplicate member => 409" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        runClientExpectStatus env status409 $ cAddMember tcAlice (grpId g) (Username "bob"),
      --
      testCase "transfer ownership" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        -- Transfer ownership to bob
        g' <- runClient env $ cTransferOwnership tcAlice (grpId g) (Username "bob")
        assertEqual "new owner" "bob" (grpOwnerName g'),
      --
      testCase "transfer ownership to non-member => 400" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status400 $ cTransferOwnership tcAlice (grpId g) (Username "bob"),
      --
      testCase "non-owner cannot transfer ownership => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        runClientExpectStatus env status403 $ cTransferOwnership tcBob (grpId g) (Username "bob"),
      --
      testCase "new owner can manage group after transfer" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        _ <- setupUser env "charlie"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        -- Transfer to bob
        _ <- runClient env $ cTransferOwnership tcAlice (grpId g) (Username "bob")
        -- Bob can now add members
        g' <- runClient env $ cAddMember tcBob (grpId g) (Username "charlie")
        assertEqual "three members" ["alice", "bob", "charlie"] (grpMemberNames g')
        -- Alice (former owner) can no longer add members
        runClientExpectStatus env status403 $ cAddMember tcAlice (grpId g) (Username "alice")
    ]

-- Receipt tests
receiptTests :: TestTree
receiptTests =
  testGroup
    "Receipts"
    [ testCase "create receipt with multiple expense records" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req =
              CreateReceiptRequest
                { note = "Restaurant bill",
                  records =
                    [ ExpenseRecordRequest
                        { title = "Dinner",
                          amount = 100.0,
                          byUsername = Username "alice",
                          date = read "2025-06-01",
                          splits =
                            [ RecordSplitRequest (Username "alice") 1,
                              RecordSplitRequest (Username "bob") 1
                            ]
                        },
                      ExpenseRecordRequest
                        { title = "Drinks",
                          amount = 40.0,
                          byUsername = Username "bob",
                          date = read "2025-06-01",
                          splits =
                            [ RecordSplitRequest (Username "alice") 1,
                              RecordSplitRequest (Username "bob") 1
                            ]
                        }
                    ]
                }
        r <- runClient env $ cCreateReceipt tc (grpId g) req
        assertEqual "note" "Restaurant bill" (receiptNote r)
        assertEqual "records count" 2 (length $ receiptRecords r)
        assertEqual "uploaded by" "alice" (userName $ uploadedBy r),
      --
      testCase "list receipts in group" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req1 =
              CreateReceiptRequest
                { note = "Receipt 1",
                  records =
                    [ ExpenseRecordRequest
                        "Lunch"
                        50.0
                        (Username "alice")
                        (read "2025-06-01")
                        [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                    ]
                }
            req2 =
              CreateReceiptRequest
                { note = "Receipt 2",
                  records =
                    [ ExpenseRecordRequest
                        "Snacks"
                        15.0
                        (Username "bob")
                        (read "2025-06-02")
                        [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                    ]
                }
        _ <- runClient env $ cCreateReceipt tc (grpId g) req1
        _ <- runClient env $ cCreateReceipt tc (grpId g) req2
        rs <- runClient env $ withPagination1 (cGetReceipts tc) (grpId g) defaultPaginationParams
        assertEqual "two receipts" 2 (length $ pageItems rs),
      --
      testCase "get receipt by id" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req =
              CreateReceiptRequest
                { note = "Solo receipt",
                  records =
                    [ ExpenseRecordRequest
                        "Coffee"
                        5.0
                        (Username "alice")
                        (read "2025-06-01")
                        [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                    ]
                }
        created <- runClient env $ cCreateReceipt tc (grpId g) req
        fetched <- runClient env $ cGetReceipt tc (grpId g) (receiptId created)
        assertEqual "note matches" "Solo receipt" (receiptNote fetched)
        assertEqual "records count" 1 (length $ receiptRecords fetched),
      --
      testCase "delete receipt deletes associated records" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        let req =
              CreateReceiptRequest
                { note = "To delete",
                  records =
                    [ ExpenseRecordRequest
                        "Item"
                        10.0
                        (Username "alice")
                        (read "2025-06-01")
                        [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                    ]
                }
        created <- runClient env $ cCreateReceipt tc (grpId g) req
        let rctId = receiptId created
            recId = extractRecordId (head $ receiptRecords created)
        _ <- runClient env $ cDeleteReceipt tc (grpId g) rctId
        -- Receipt should be gone
        runClientExpectStatus env status404 $ cGetReceipt tc (grpId g) rctId
        -- The record should also be gone
        runClientExpectStatus env status404 $ cGetRecord tc (grpId g) recId,
      --
      testCase "non-member cannot create receipt => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let req =
              CreateReceiptRequest
                { note = "Unauthorized",
                  records =
                    [ ExpenseRecordRequest
                        "Unauthorized item"
                        10.0
                        (Username "alice")
                        (read "2025-06-01")
                        [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                    ]
                }
        runClientExpectStatus env status403 $ cCreateReceipt tcBob (grpId g) req,
      --
      testCase "receipt records appear in group records listing" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cAddMember tc (grpId g) (Username "bob")
        -- Create a receipt with one record
        let req =
              CreateReceiptRequest
                { note = "Via receipt",
                  records =
                    [ ExpenseRecordRequest
                        "Taxi"
                        25.0
                        (Username "alice")
                        (read "2025-06-01")
                        [RecordSplitRequest (Username "alice") 1, RecordSplitRequest (Username "bob") 1]
                    ]
                }
        _ <- runClient env $ cCreateReceipt tc (grpId g) req
        -- The record should also appear in the group's record listing
        rs <- runClient env $ withPagination1 (cGetRecords tc) (grpId g) defaultPaginationParams
        assertEqual "one record in group" 1 (length $ pageItems rs)
    ]
