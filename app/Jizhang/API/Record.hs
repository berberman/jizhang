{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Record where

import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, maybeToList)
import Jizhang.API.Types
import Jizhang.API.Utils
import Jizhang.Common.MyUUID
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

type RecordAPI =
  -- Add a new expense record to a group
  "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> ReqBody '[JSON] ExpenseRecordRequest :> Post '[JSON] Record
    -- Add a new transfer record to a group
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> ReqBody '[JSON] TransferRecordRequest :> Post '[JSON] Record
    -- Get a specific record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> Get '[JSON] Record
    -- Get all records in a group
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Get '[JSON] [Record]
    -- Delete a specific record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> DeleteNoContent
    -- Update a specific transfer record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> Capture "recordId" RecordId :> ReqBody '[JSON] TransferRecordRequest :> Put '[JSON] Record
    -- Update a specific expense record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> Capture "recordId" RecordId :> ReqBody '[JSON] ExpenseRecordRequest :> Put '[JSON] Record

recordServer :: MyServer RecordAPI
recordServer =
  addExpenseRecord
    :<|> addTransferRecord
    :<|> getRecord
    :<|> getRecordsInGroup
    :<|> deleteRecord
    :<|> updateTransfer
    :<|> updateExpense

addExpenseRecord :: GroupId -> ExpenseRecordRequest -> MyHandler Record
addExpenseRecord (GroupId gId) req@ExpenseRecordRequest {..} = do
  logInfo_ $ "Adding expense record to group " <> uuidToText gId
  ensureGroupExists gId
  validateExpenseRecordRequest req
  -- Add the record
  record <- runDB $ D.insertRecord title amount (coerce byUsername) Nothing gId at
  -- Add the splits with split amounts calculated
  ss <- forM splits $ \RecordSplitRequest {..} ->
    runDB $ D.insertRecordSplit (S._recordId record) (coerce username) percentage (fromIntegral percentage * amount / 100)
  pure $ recordToExpenseRecord record ss

addTransferRecord :: GroupId -> TransferRecordRequest -> MyHandler Record
addTransferRecord (GroupId gId) req@TransferRecordRequest {..} = do
  logInfo_ $ "Adding transfer record to group " <> uuidToText gId
  ensureGroupExists gId
  validateTransferRecordRequest req
  record <- runDB $ D.insertRecord "Transfer" amount (coerce byUsername) (Just (coerce toUsername)) gId at
  pure $ recordToTransferRecord record

getRecord :: GroupId -> RecordId -> MyHandler Record
getRecord (GroupId gId) (RecordId rId) = do
  -- We actually don't need group to get the record, but we still check it exists
  logInfo_ $ "Fetching record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
  ensureGroupExists gId
  ensureRecordExists rId
  (record, ssplits) <- fromJust <$> runDB (D.getRecordWithSplits rId)
  pure $
    if S.isTransferRecord record
      then recordToTransferRecord record
      else recordToExpenseRecord record ssplits

getRecordsInGroup :: GroupId -> MyHandler [Record]
getRecordsInGroup (GroupId gId) = do
  logInfo_ $ "Fetching all records in group " <> uuidToText gId
  ensureGroupExists gId
  rs <- runDB $ D.getRecordsWithSplitsForGroup gId
  let mp = M.fromListWith (++) [(r, maybeToList s) | (r, s) <- rs]
  pure [if S.isTransferRecord r then recordToTransferRecord r else recordToExpenseRecord r ss | (r, ss) <- M.toList mp]

deleteRecord :: GroupId -> RecordId -> MyHandler NoContent
deleteRecord (GroupId gId) (RecordId rId) = do
  -- Again we don't need group to delete the record, but we still check it exists
  logInfo_ $ "Deleting record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
  ensureGroupExists gId
  ensureRecordExists rId
  runDB $ D.deleteRecord rId
  pure NoContent

updateTransfer :: GroupId -> RecordId -> TransferRecordRequest -> MyHandler Record
updateTransfer (GroupId gId) (RecordId rId) req@TransferRecordRequest {..} = do
  logInfo_ $ "Updating transfer record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
  old <- getRecord (GroupId gId) (RecordId rId)
  case old of
    TransferRecord {} -> do
      validateTransferRecordRequest req
      _ <- runDB $ D.updateRecord rId Nothing (Just amount) (Just $ coerce byUsername) (Just $ Just $ coerce toUsername) (Just at)
      getRecord (GroupId gId) (RecordId rId)
    _ -> throwError $ err400 {errBody = "Record is not a transfer record"}

updateExpense :: GroupId -> RecordId -> ExpenseRecordRequest -> MyHandler Record
updateExpense (GroupId gId) (RecordId rId) req@ExpenseRecordRequest {..} = do
  logInfo_ $ "Updating expense record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
  old <- getRecord (GroupId gId) (RecordId rId)
  case old of
    ExpenseRecord {recordId = rid} -> do
      validateExpenseRecordRequest req
      -- Delete existing splits
      runDB $ D.deleteRecordSplitsForRecord rId
      -- Update the record
      runDB $ D.updateRecord rId (Just title) (Just amount) (Just $ coerce byUsername) Nothing (Just at)
      -- Add new splits with split amounts calculated
      forM_ splits $ \RecordSplitRequest {..} ->
        runDB $ D.insertRecordSplit (coerce rid) (coerce username) percentage (fromIntegral percentage * amount / 100)
      -- Return the updated record
      getRecord (GroupId gId) (RecordId rId)
    _ -> throwError $ err400 {errBody = "Record is not an expense record"}
