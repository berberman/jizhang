{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Member-authorized ledger routes. Record IDs are resolved within the captured
-- group, and financial mutations validate requests before locking currency policy.
module Jizhang.API.Record (RecordAPI, recordServer) where

import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.UUID (toText)
import Jizhang.API.App
import Jizhang.API.Internal.Error (fetchOrFail, fromStoredData, textToLBS)
import Jizhang.API.Record.Projection (recordToApiRecord, recordToSplitRecord, recordToTransferRecord)
import Jizhang.API.Record.Input (resolveExpenseRequest, resolveIncomeRequest, resolveTransferRequest, withGroupCurrencyLock)
import Jizhang.API.Record.Service (getRecordPage)
import Jizhang.API.Common
import Jizhang.API.Types
import Jizhang.API.Internal.Resource (ensureGroupExists, ensureGroupMember, ensureRecordExists, getGroupUserMap)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

-- | Expense, income and transfer creation and replacement share the group ledger;
-- replacement routes reject an existing record of a different kind with 400.
type RecordAPI =
  "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> ReqBody '[JSON] ExpenseRecordRequest :> Post '[JSON] Record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> ReqBody '[JSON] TransferRecordRequest :> Post '[JSON] Record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> Get '[JSON] Record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> WithPagination (Get '[JSON] (PaginatedResponse Record))
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> Delete '[JSON] NoContent
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> Capture "recordId" RecordId :> ReqBody '[JSON] TransferRecordRequest :> Put '[JSON] Record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> Capture "recordId" RecordId :> ReqBody '[JSON] ExpenseRecordRequest :> Put '[JSON] Record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "income" :> ReqBody '[JSON] IncomeRecordRequest :> Post '[JSON] Record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "income" :> Capture "recordId" RecordId :> ReqBody '[JSON] IncomeRecordRequest :> Put '[JSON] Record

recordServer :: AuthUser -> MyServer RecordAPI
recordServer authUser =
  addExpenseRecord authUser
    :<|> addTransferRecord authUser
    :<|> getRecord authUser
    :<|> getRecordsInGroup authUser
    :<|> deleteRecord authUser
    :<|> updateTransfer authUser
    :<|> updateExpense authUser
    :<|> addIncomeRecord authUser
    :<|> updateIncome authUser

addExpenseRecord :: AuthUser -> GroupId -> ExpenseRecordRequest -> MyHandler Record
addExpenseRecord = addSplitRecord D.ExpenseKind resolveExpenseRequest

addIncomeRecord :: AuthUser -> GroupId -> IncomeRecordRequest -> MyHandler Record
addIncomeRecord = addSplitRecord D.IncomeKind resolveIncomeRequest

-- | Resolve participants and allocations before the atomic currency-locked insert.
addSplitRecord :: D.SplitRecordKind -> (S.Group -> request -> MyHandler D.ResolvedSplitRecord) -> AuthUser -> GroupId -> request -> MyHandler Record
addSplitRecord kind resolveRequest authUser (GroupId gId) req = do
  logInfo_ $ "Adding " <> D.splitRecordKindText kind <> " record to group " <> toText gId
  group <- fst <$> fetchOrFail "Group" gId (runDB $ D.getGroupWithMembers gId)
  ensureGroupMember (authUserId authUser) gId
  resolved <- resolveRequest group req
  (record, ss) <- withGroupCurrencyLock group $ D.insertSplitRecord kind gId Nothing resolved
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  fromStoredData $ recordToSplitRecord kind currencies um record ss

addTransferRecord :: AuthUser -> GroupId -> TransferRecordRequest -> MyHandler Record
addTransferRecord authUser (GroupId gId) req@TransferRecordRequest {..} = do
  logInfo_ $ "Adding transfer record to group " <> toText gId
  group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
  ensureGroupMember (authUserId authUser) gId
  resolved <- resolveTransferRequest group req
  record <- withGroupCurrencyLock group $ D.insertTransferRecord gId resolved
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  fromStoredData $ recordToTransferRecord currencies um (coerce toUserId) record

-- | Authorize the group before projecting persisted amounts and historical users.
getRecord :: AuthUser -> GroupId -> RecordId -> MyHandler Record
getRecord authUser (GroupId gId) (RecordId rId) = do
  logTrace_ $ "Fetching record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  (record, ssplits) <- fetchOrFail "Record" rId $ runDB (D.getRecordWithSplits gId rId)
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  fromStoredData $ recordToApiRecord currencies um record ssplits

-- | Membership is checked here; the shared service filters and sorts before slicing.
getRecordsInGroup :: AuthUser -> GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Record)
getRecordsInGroup authUser group@(GroupId gId) mQuery mOffset mLimit mSort = do
  logTrace_ $ "Fetching all records in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  getRecordPage group mQuery mOffset mLimit mSort

deleteRecord :: AuthUser -> GroupId -> RecordId -> MyHandler NoContent
deleteRecord authUser (GroupId gId) (RecordId rId) = do
  logInfo_ $ "Deleting record with ID " <> toText rId <> " in group " <> toText gId
  group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
  ensureGroupMember (authUserId authUser) gId
  ensureRecordExists gId rId
  withGroupCurrencyLock group $ D.deleteRecord gId rId
  pure NoContent

updateTransfer :: AuthUser -> GroupId -> RecordId -> TransferRecordRequest -> MyHandler Record
updateTransfer authUser (GroupId gId) (RecordId rId) req = do
  logInfo_ $ "Updating transfer record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  old <- getRecord authUser (GroupId gId) (RecordId rId)
  case old of
    TransferRecord {} -> do
      group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
      resolved <- resolveTransferRequest group req
      withGroupCurrencyLock group $ D.replaceTransferRecord gId rId resolved
      getRecord authUser (GroupId gId) (RecordId rId)
    _ -> throwError $ err400 {errBody = "Record is not a transfer record"}

updateExpense :: AuthUser -> GroupId -> RecordId -> ExpenseRecordRequest -> MyHandler Record
updateExpense = updateSplitRecord D.ExpenseKind resolveExpenseRequest

updateIncome :: AuthUser -> GroupId -> RecordId -> IncomeRecordRequest -> MyHandler Record
updateIncome = updateSplitRecord D.IncomeKind resolveIncomeRequest

-- | Preserve the record kind while replacing its validated monetary allocations.
updateSplitRecord :: D.SplitRecordKind -> (S.Group -> request -> MyHandler D.ResolvedSplitRecord) -> AuthUser -> GroupId -> RecordId -> request -> MyHandler Record
updateSplitRecord kind resolveRequest authUser gid@(GroupId gId) rid@(RecordId rId) req = do
  logInfo_ $ "Updating " <> D.splitRecordKindText kind <> " record with ID " <> toText rId <> " in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  old <- getRecord authUser gid rid
  case (kind, old) of
    (D.ExpenseKind, ExpenseRecord {}) -> replace
    (D.IncomeKind, IncomeRecord {}) -> replace
    _ -> throwError $ err400 {errBody = "Record is not an " <> textToLBS (D.splitRecordKindText kind) <> " record"}
  where
    replace = do
      group <- fst <$> fetchOrFail "Group" gId (runDB $ D.getGroupWithMembers gId)
      resolved <- resolveRequest group req
      withGroupCurrencyLock group $ D.replaceSplitRecord kind gId rId resolved
      getRecord authUser gid rid
