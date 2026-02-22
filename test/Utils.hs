{-# LANGUAGE RecordWildCards #-}

module Utils where

import Data.List (sort)
import Data.Text (Text)
import Jizhang.API.Types
import Network.HTTP.Types.Status
import Servant.Client
import Test.Tasty.HUnit

-- | Assert that a Double is approximately equal
assertApproxEqual :: String -> Double -> Double -> Assertion
assertApproxEqual msg expected actual =
  assertBool
    (msg ++ ": expected " ++ show expected ++ " but got " ++ show actual)
    (abs (expected - actual) < 1e-9)

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

extractRecordId :: Record -> RecordId
extractRecordId ExpenseRecord {..} = recordId
extractRecordId TransferRecord {..} = recordId

receiptNote :: Receipt -> Text
receiptNote (Receipt _ _ _ n _ _) = n

receiptRecords :: Receipt -> [Record]
receiptRecords (Receipt _ _ _ _ rs _) = rs
