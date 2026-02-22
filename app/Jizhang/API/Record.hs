{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Record where

import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.UUID (toText)
import Jizhang.API.Types
import Jizhang.API.Utils
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
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> Delete '[JSON] NoContent
    -- Update a specific transfer record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> Capture "recordId" RecordId :> ReqBody '[JSON] TransferRecordRequest :> Put '[JSON] Record
    -- Update a specific expense record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> Capture "recordId" RecordId :> ReqBody '[JSON] ExpenseRecordRequest :> Put '[JSON] Record

recordServer :: AuthUser -> MyServer RecordAPI
recordServer authUser =
  addExpenseRecord authUser
    :<|> addTransferRecord authUser
    :<|> getRecord authUser
    :<|> getRecordsInGroup authUser
    :<|> deleteRecord authUser
    :<|> updateTransfer authUser
    :<|> updateExpense authUser

addExpenseRecord :: AuthUser -> GroupId -> ExpenseRecordRequest -> MyHandler Record
addExpenseRecord authUser (GroupId gId) req@ExpenseRecordRequest {..} = do
  logInfo_ $ "Adding expense record to group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  validateExpenseRecordRequest gId req
  payer <- lookupUser (coerce byUsername)
  splitUsers <- forM splits $ \RecordSplitRequest {..} -> do
    u <- lookupUser (coerce username)
    pure (S._userId u, share)
  (record, ss) <- runDB $ do
    rec <- D.insertRecord title amount (S._userId payer) Nothing gId date Nothing
    ss <- forM splitUsers $ \(uid, sh) ->
      D.insertRecordSplit (S._recordId rec) uid sh
    pure (rec, ss)
  um <- getGroupUserMap gId
  pure $ recordToExpenseRecord um record ss

addTransferRecord :: AuthUser -> GroupId -> TransferRecordRequest -> MyHandler Record
addTransferRecord authUser (GroupId gId) req@TransferRecordRequest {..} = do
  logInfo_ $ "Adding transfer record to group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  validateTransferRecordRequest gId req
  payer <- lookupUser (coerce byUsername)
  receiver <- lookupUser (coerce toUsername)
  record <- runDB $ D.insertRecord "Transfer" amount (S._userId payer) (Just (S._userId receiver)) gId date Nothing
  um <- getGroupUserMap gId
  pure $ recordToTransferRecord um record

getRecord :: AuthUser -> GroupId -> RecordId -> MyHandler Record
getRecord authUser (GroupId gId) (RecordId rId) = do
  -- We actually don't need group to get the record, but we still check it exists
  logInfo_ $ "Fetching record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  (record, ssplits) <- fetchOrFail "Record" rId $ runDB (D.getRecordWithSplits rId)
  um <- getGroupUserMap gId
  pure $
    if S.isTransferRecord record
      then recordToTransferRecord um record
      else recordToExpenseRecord um record ssplits

getRecordsInGroup :: AuthUser -> GroupId -> MyHandler [Record]
getRecordsInGroup authUser (GroupId gId) = do
  logInfo_ $ "Fetching all records in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  rs <- runDB $ D.getRecordsWithSplitsForGroup gId
  um <- getGroupUserMap gId
  let mp = M.fromListWith (++) [(r, maybeToList s) | (r, s) <- rs]
  pure $
    map (\(r, ss) -> if S.isTransferRecord r then recordToTransferRecord um r else recordToExpenseRecord um r ss) $
      sortOn (S._createdAt . fst) $
        M.toList mp

deleteRecord :: AuthUser -> GroupId -> RecordId -> MyHandler NoContent
deleteRecord authUser (GroupId gId) (RecordId rId) = do
  -- Again we don't need group to delete the record, but we still check it exists
  logInfo_ $ "Deleting record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  ensureRecordExists rId
  runDB $ D.deleteRecord rId
  pure NoContent

updateTransfer :: AuthUser -> GroupId -> RecordId -> TransferRecordRequest -> MyHandler Record
updateTransfer authUser (GroupId gId) (RecordId rId) req@TransferRecordRequest {..} = do
  logInfo_ $ "Updating transfer record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  old <- getRecord authUser (GroupId gId) (RecordId rId)
  case old of
    TransferRecord {} -> do
      validateTransferRecordRequest gId req
      payer <- lookupUser (coerce byUsername)
      receiver <- lookupUser (coerce toUsername)
      _ <- runDB $ D.updateRecord rId Nothing (Just amount) (Just $ S._userId payer) (Just $ Just $ S._userId receiver) (Just date)
      getRecord authUser (GroupId gId) (RecordId rId)
    _ -> throwError $ err400 {errBody = "Record is not a transfer record"}

updateExpense :: AuthUser -> GroupId -> RecordId -> ExpenseRecordRequest -> MyHandler Record
updateExpense authUser (GroupId gId) (RecordId rId) req@ExpenseRecordRequest {..} = do
  logInfo_ $ "Updating expense record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  old <- getRecord authUser (GroupId gId) (RecordId rId)
  case old of
    ExpenseRecord {recordId = rid} -> do
      validateExpenseRecordRequest gId req
      payer <- lookupUser (coerce byUsername)
      splitUsers <- forM splits $ \RecordSplitRequest {..} -> do
        u <- lookupUser (coerce username)
        pure (S._userId u, share)
      runDB $ do
        D.deleteRecordSplitsForRecord rId
        D.updateRecord rId (Just title) (Just amount) (Just $ S._userId payer) Nothing (Just date)
        forM_ splitUsers $ \(uid, sh) ->
          D.insertRecordSplit (coerce rid) uid sh
      getRecord authUser (GroupId gId) (RecordId rId)
    _ -> throwError $ err400 {errBody = "Record is not an expense record"}
