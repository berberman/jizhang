{-# LANGUAGE RecordWildCards #-}

-- | Assertions and DTO projections shared by HTTP integration tests. These helpers
-- do not initialize storage; callers own application and database lifetimes.
module Utils where

import Data.List (sort)
import Data.Text (Text)
import Jizhang.API.Types
import Network.HTTP.Types.Status
import Servant.Client
import Test.Tasty.HUnit

-- | Treat any client failure as a test failure, including transport/decode errors.
-- Diagnostics render ClientError, so avoid this helper for secret-bearing failures.
runClient :: ClientEnv -> ClientM a -> IO a
runClient env action = do
  result <- runClientM action env
  case result of
    Left err -> assertFailure ("Client error: " ++ show err) >> undefined
    Right a -> pure a

-- | Require an HTTP failure with the exact status; transport and decoding failures
-- cannot satisfy an expected application rejection.
runClientExpectStatus :: ClientEnv -> Status -> ClientM a -> IO ()
runClientExpectStatus env expectedStatus action = do
  result <- runClientM action env
  case result of
    Left (FailureResponse _ resp) ->
      assertEqual "status code" expectedStatus (responseStatusCode resp)
    Left err -> assertFailure ("Expected FailureResponse but got: " ++ show err)
    Right _ -> assertFailure "Expected failure but got success"

userName :: User -> Text
userName (User _ n _) = n

userIdOf :: User -> UserId
userIdOf (User value _ _) = value

grpId :: Group -> GroupId
grpId Group {groupId = gid} = gid

grpName :: Group -> Text
grpName Group {groupName = n} = n

grpOwnerName :: Group -> Text
grpOwnerName Group {owner = o} = userName o

-- | Sort member names so assertions do not depend on server membership ordering.
grpMemberNames :: Group -> [Text]
grpMemberNames Group {members = ms} = sort $ map userName ms

-- | Project the shared record identifier without assuming a financial record kind.
extractRecordId :: Record -> RecordId
extractRecordId ExpenseRecord {..} = recordId
extractRecordId TransferRecord {..} = recordId
extractRecordId IncomeRecord {..} = recordId

receiptNote :: Receipt -> Text
receiptNote (Receipt _ _ _ n _ _) = n

receiptRecords :: Receipt -> [Record]
receiptRecords (Receipt _ _ _ _ rs _) = rs
