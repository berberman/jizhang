{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module APITest (apiTests) where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute_)
import Jizhang.API (app)
import Jizhang.API.Auth
import Jizhang.API.Group
import Jizhang.API.Import
import Jizhang.API.Receipt
import Jizhang.API.Record
import Jizhang.API.Report
import Jizhang.API.Types
import Jizhang.API.User
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
import Utils

-- Unprotected auth endpoints
register_ :: RegisterRequest -> ClientM User
login_ :: LoginRequest -> ClientM LoginResponse
(register_ :<|> login_) = client (Proxy :: Proxy AuthAPI)

-- Protected endpoints (require JWT auth token)
type TestProtectedAPI =
  Auth '[JWT] AuthUser :> (UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ImportAPI :<|> ReceiptAPI)

protectedClient_ ::
  Token ->
  Client ClientM (UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ImportAPI :<|> ReceiptAPI)
protectedClient_ = client (Proxy :: Proxy TestProtectedAPI)

-- Convenience record holding all destructured client functions
data TC = TC
  { cGetMe :: ClientM User,
    cDeleteMe :: ClientM NoContent,
    cGetMyGroups :: ClientM [Group],
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
    cGetRecords :: GroupId -> ClientM [Record],
    cDeleteRecord :: GroupId -> RecordId -> ClientM NoContent,
    cUpdateTransfer :: GroupId -> RecordId -> TransferRecordRequest -> ClientM Record,
    cUpdateExpense :: GroupId -> RecordId -> ExpenseRecordRequest -> ClientM Record,
    cGetReport :: GroupId -> ClientM Report,
    cCreateReceipt :: GroupId -> CreateReceiptRequest -> ClientM Receipt,
    cGetReceipts :: GroupId -> ClientM [Receipt],
    cGetReceipt :: GroupId -> ReceiptId -> ClientM Receipt,
    cDeleteReceipt :: GroupId -> ReceiptId -> ClientM NoContent,
    cUpdateReceipt :: GroupId -> ReceiptId -> UpdateReceiptRequest -> ClientM Receipt
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

withTestApp :: (ClientEnv -> IO a) -> IO a
withTestApp action = withStdOutLogger $ \logger -> do
  connStr <- fromMaybe "dbname=jizhang_test" . fmap pack <$> lookupEnv "TEST_DATABASE_URL"
  conn <- connectPostgreSQL connStr
  dropTestTables conn
  createTestTables conn
  jwk <- generateKey
  let jwtCfg = defaultJWTSettings jwk
      appEnv = AppEnv conn jwtCfg
  waiApp <- runLogT "test" logger maxBound $ app appEnv
  manager <- newManager defaultManagerSettings
  testWithApplication (pure waiApp) $ \port -> do
    let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
    result <- action env
    dropTestTables conn
    close conn
    pure result

dropTestTables :: Connection -> IO ()
dropTestTables conn = do
  _ <- execute_ conn "DROP TABLE IF EXISTS record_splits CASCADE;"
  _ <- execute_ conn "DROP TABLE IF EXISTS records CASCADE;"
  _ <- execute_ conn "DROP TABLE IF EXISTS receipts CASCADE;"
  _ <- execute_ conn "DROP TABLE IF EXISTS group_members CASCADE;"
  _ <- execute_ conn "DROP TABLE IF EXISTS groups CASCADE;"
  _ <- execute_ conn "DROP TABLE IF EXISTS users CASCADE;"
  pure ()

createTestTables :: Connection -> IO ()
createTestTables conn = do
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS users (id UUID PRIMARY KEY, username TEXT NOT NULL UNIQUE, password_hash TEXT NOT NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS groups (id UUID PRIMARY KEY, name TEXT NOT NULL, owner__id UUID NOT NULL REFERENCES users (id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS group_members (user__id UUID REFERENCES users (id) ON DELETE CASCADE, group__id UUID REFERENCES groups (id) ON DELETE CASCADE, active BOOLEAN NOT NULL DEFAULT TRUE, PRIMARY KEY (user__id, group__id));"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS receipts (id UUID PRIMARY KEY, group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE, uploaded_by__id UUID NOT NULL REFERENCES users (id), note TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS records (id UUID PRIMARY KEY, group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE, title TEXT NOT NULL, amount DOUBLE PRECISION NOT NULL, by__id UUID NOT NULL REFERENCES users (id), to__id UUID REFERENCES users (id), date DATE NOT NULL, created_at TIMESTAMPTZ NOT NULL, receipt__id UUID REFERENCES receipts (id) ON DELETE SET NULL);"
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS record_splits (record__id UUID REFERENCES records (id) ON DELETE CASCADE, user__id UUID REFERENCES users (id), share SMALLINT NOT NULL CHECK (share >= 0), PRIMARY KEY (record__id, user__id));"
  pure ()

-- | Register a user and log in, return session token
setupUser :: ClientEnv -> Text -> IO Text
setupUser env name = runClient env $ do
  _ <- register_ (RegisterRequest name "password123")
  resp <- login_ (LoginRequest name "password123")
  pure resp.accessToken

apiTests :: TestTree
apiTests =
  testGroup
    "API Integration"
    [ authTests,
      userTests,
      groupTests,
      recordTests,
      reportIntegrationTests,
      authorizationTests,
      receiptTests
    ]

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
      testCase "register duplicate => 409" $ withTestApp $ \env -> do
        _ <- runClient env $ register_ (RegisterRequest "alice" "password123")
        runClientExpectStatus env status409 $ register_ (RegisterRequest "alice" "password123"),
      testCase "login wrong password => 401" $ withTestApp $ \env -> do
        _ <- runClient env $ register_ (RegisterRequest "alice" "password123")
        runClientExpectStatus env status401 $ login_ (LoginRequest "alice" "wrongpassword"),
      testCase "login nonexistent user => 401" $ withTestApp $ \env -> do
        runClientExpectStatus env status401 $ login_ (LoginRequest "nobody" "password123"),
      testCase "protected endpoint without token => 401" $ withTestApp $ \env -> do
        let tc = mkTC "invalid-token"
        runClientExpectStatus env status401 (cGetMe tc)
    ]

userTests :: TestTree
userTests =
  testGroup
    "Users"
    [ testCase "get me" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        u <- runClient env $ cGetMe tc
        assertEqual "username" "alice" (userName u),
      testCase "delete me" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        let tc = mkTC tokAlice
        _ <- runClient env $ cDeleteMe tc
        -- Token is now invalid (user gone), so /users/me should fail
        runClientExpectStatus env status404 $ cGetMe tc
    ]

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
      testCase "cannot deactivate group owner" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        runClientExpectStatus env status400 $ cDeleteMember tc (grpId g) (Username "alice"),
      testCase "delete group cascades" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Trip"
        _ <- runClient env $ cDeleteGroupById tc (grpId g)
        runClientExpectStatus env status404 $ cGetGroupById tc (grpId g),
      testCase "update group name" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        g <- runClient env $ cCreateGroup tc "Old Name"
        g' <- runClient env $ cUpdateGroupById tc (grpId g) "New Name"
        assertEqual "updated" "New Name" (grpName g'),
      testCase "my groups endpoint" $ withTestApp $ \env -> do
        tok <- setupUser env "alice"
        let tc = mkTC tok
        _ <- runClient env $ cCreateGroup tc "Trip1"
        _ <- runClient env $ cCreateGroup tc "Trip2"
        gs <- runClient env $ cGetMyGroups tc
        assertEqual "two groups" 2 (length gs)
    ]

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
        rs <- runClient env $ cGetRecords tc (grpId g)
        assertEqual "two records" 2 (length rs),
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
      testCase "non-owner cannot add member => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        _ <- setupUser env "charlie"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        -- Bob (not owner) tries to add charlie
        runClientExpectStatus env status403 $ cAddMember tcBob (grpId g) (Username "charlie"),
      testCase "non-owner cannot update group => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cUpdateGroupById tcBob (grpId g) "Hacked",
      testCase "non-owner cannot delete group => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cDeleteGroupById tcBob (grpId g),
      testCase "non-member cannot add records => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        let req = TransferRecordRequest 10.0 (Username "alice") (Username "bob") (read "2025-06-01")
        runClientExpectStatus env status403 $ cAddTransfer tcBob (grpId g) req,
      testCase "non-member cannot view records => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cGetRecords tcBob (grpId g),
      testCase "non-member cannot view report => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status403 $ cGetReport tcBob (grpId g),
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
      testCase "adding duplicate member => 409" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        runClientExpectStatus env status409 $ cAddMember tcAlice (grpId g) (Username "bob"),
      testCase "transfer ownership" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        -- Transfer ownership to bob
        g' <- runClient env $ cTransferOwnership tcAlice (grpId g) (Username "bob")
        assertEqual "new owner" "bob" (grpOwnerName g'),
      testCase "transfer ownership to non-member => 400" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        runClientExpectStatus env status400 $ cTransferOwnership tcAlice (grpId g) (Username "bob"),
      testCase "transfer ownership to deactivated member => 400" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        _ <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        _ <- runClient env $ cDeleteMember tcAlice (grpId g) (Username "bob")
        -- Bob is now deactivated; transfer should fail
        runClientExpectStatus env status400 $ cTransferOwnership tcAlice (grpId g) (Username "bob"),
      testCase "non-owner cannot transfer ownership => 403" $ withTestApp $ \env -> do
        tokAlice <- setupUser env "alice"
        tokBob <- setupUser env "bob"
        let tcAlice = mkTC tokAlice
            tcBob = mkTC tokBob
        g <- runClient env $ cCreateGroup tcAlice "Trip"
        _ <- runClient env $ cAddMember tcAlice (grpId g) (Username "bob")
        runClientExpectStatus env status403 $ cTransferOwnership tcBob (grpId g) (Username "bob"),
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
        rs <- runClient env $ cGetReceipts tc (grpId g)
        assertEqual "two receipts" 2 (length rs),
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
        rs <- runClient env $ cGetRecords tc (grpId g)
        assertEqual "one record in group" 1 (length rs)
    ]
