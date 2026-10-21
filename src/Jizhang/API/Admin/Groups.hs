{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Operator group and ledger maintenance bypasses member/owner authorization while
-- retaining parent-group scoping, participant validation and financial invariants.
module Jizhang.API.Admin.Groups
  ( getAllGroups,
    createGroup,
    bulkDeleteGroups,
    getGroupById,
    updateGroup,
    deleteGroup,
    addGroupMember,
    bulkAddGroupMembers,
    deleteGroupMember,
    bulkDeleteGroupMembers,
    transferOwnership,
    importGroupCSV,
    addExpenseRecord,
    addIncomeRecord,
    addTransferRecord,
    getGroupRecords,
    getGroupRecord,
    bulkDeleteRecords,
    deleteRecord,
    updateTransfer,
    updateExpense,
    updateIncome,
    getGroupReport,
    createReceipt,
    getGroupReceipts,
    bulkDeleteReceipts,
    deleteReceipt,
    updateReceipt,
  )
where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Jizhang.API.Admin.Common
import Jizhang.API.Group (getGroup)
import Jizhang.API.GroupImport
import Jizhang.API.Receipt (getReceiptsByGroupId)
import Jizhang.API.Record.Service (addExpenseRecords, getRecordPage)
import Jizhang.API.Report (getReportByGroupId)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (AdminPrincipal))
import Jizhang.API.Types
import Jizhang.API.Internal.Error (fromStoredData, fetchOrFail)
import Jizhang.API.User.Projection (resolveUser)
import Jizhang.API.Record.Projection (recordToSplitRecord, recordToTransferRecord, recordToApiRecord)
import Jizhang.API.Record.Input (resolveExpenseRequest, resolveIncomeRequest, resolveTransferRequest, inputCurrency, withGroupCurrencyLock)
import Jizhang.API.Internal.Resource (lookupUserById, getGroupUserMap, ensureGroupExists, validateGroupName, ensureActiveGroupUser, ensureRecordExists, ensureReceiptExists)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

getAllGroups :: AuthAdmin -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Group)
getAllGroups authAdmin mQuery mOffset mLimit mSort =
  paginatedListHandler
    ("Listing all groups for admin: " <> authAdminUsername authAdmin)
    loadAllGroups
    id
    filterGroups
    sortGroups
    mQuery
    mOffset
    mLimit
    mSort

-- | Create the chosen owner's active membership atomically with the new group.
createGroup :: AuthAdmin -> AdminCreateGroupRequest -> MyHandler Group
createGroup authAdmin AdminCreateGroupRequest {..} = do
  logInfo_ $ "Creating group as admin: " <> authAdminUsername authAdmin <> " name=" <> createGroupName
  validateGroupName createGroupName
  currencies <- asks appCurrencies
  _ <- inputCurrency currencies defaultCurrency
  ownerUser <- lookupUserById (coerce ownerUserId)
  let ownerUid = S._userId ownerUser
  group <- runDBTransaction $ do
    group <- D.insertGroup createGroupName ownerUid currencyMode defaultCurrency
    _ <- D.addGroupMember ownerUid (S._groupId group)
    pure group
  getGroup (coerce $ S._groupId group)

bulkDeleteGroups :: AuthAdmin -> BulkRequest GroupId -> MyHandler NoContent
bulkDeleteGroups authAdmin (BulkRequest groupIds) =
  bulkDeleteHandler
    ("Bulk deleting groups as admin: " <> authAdminUsername authAdmin)
    groupIds
    (\(GroupId gid) -> ensureGroupExists gid)
    (\(GroupId gid) -> D.deleteGroup gid)

getGroupById :: AuthAdmin -> GroupId -> MyHandler Group
getGroupById authAdmin gid = do
  logTrace_ $ "Fetching group for admin: " <> authAdminUsername authAdmin
  getGroup gid

updateGroup :: AuthAdmin -> GroupId -> UpdateGroupRequest -> MyHandler Group
updateGroup authAdmin (GroupId gid) UpdateGroupRequest {..} = do
  logInfo_ $ "Updating group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  validateGroupName groupName
  currencies <- asks appCurrencies
  _ <- inputCurrency currencies defaultCurrency
  updated <- runDB $ D.updateGroupSettings gid groupName currencyMode defaultCurrency
  case updated of
    Nothing -> throwError $ err409 {errBody = "Currency settings are locked after the first record"}
    Just _ -> getGroup (GroupId gid)

deleteGroup :: AuthAdmin -> GroupId -> MyHandler NoContent
deleteGroup authAdmin (GroupId gid) = do
  logInfo_ $ "Deleting group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  runDB $ D.deleteGroup gid
  pure NoContent

addGroupMember :: AuthAdmin -> GroupId -> UserId -> MyHandler Group
addGroupMember authAdmin (GroupId gid) userId@(UserId uid) = do
  logInfo_ $ "Adding member " <> toText uid <> " to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  (sUser, existingRow) <- resolveAddableGroupMember gid userId
  if existingRow
    then runDB $ D.reactivateGroupMember (S._userId sUser) gid
    else void $ runDB $ D.addGroupMember (S._userId sUser) gid
  getGroup (GroupId gid)

-- | Reject duplicate or already-active targets before atomically adding/reactivating all.
bulkAddGroupMembers :: AuthAdmin -> GroupId -> BulkRequest UserId -> MyHandler Group
bulkAddGroupMembers authAdmin (GroupId gid) (BulkRequest userIds) = do
  logInfo_ $ "Bulk adding members to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  ensureDistinctMembers userIds
  resolved <- mapM (resolveAddableGroupMember gid) userIds
  runDBTransaction $ forM_ resolved $ \(sUser, existingRow) ->
    if existingRow
      then D.reactivateGroupMember (S._userId sUser) gid
      else void $ D.addGroupMember (S._userId sUser) gid
  getGroup (GroupId gid)

deleteGroupMember :: AuthAdmin -> GroupId -> UserId -> MyHandler NoContent
deleteGroupMember authAdmin (GroupId gid) userId@(UserId uid) = do
  logInfo_ $ "Removing member " <> toText uid <> " from group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  targetUid <- resolveRemovableGroupMember gid userId
  runDB $ D.deactivateGroupMember targetUid gid
  pure NoContent

-- | Validate every target before atomically deactivating memberships; owners cannot leave.
bulkDeleteGroupMembers :: AuthAdmin -> GroupId -> BulkRequest UserId -> MyHandler NoContent
bulkDeleteGroupMembers authAdmin (GroupId gid) (BulkRequest userIds) = do
  logInfo_ $ "Bulk removing members from group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  ensureDistinctMembers userIds
  resolved <- mapM (resolveRemovableGroupMember gid) userIds
  runDBTransaction $ forM_ resolved $ \uid -> D.deactivateGroupMember uid gid
  pure NoContent

transferOwnership :: AuthAdmin -> GroupId -> UserId -> MyHandler Group
transferOwnership authAdmin (GroupId gid) (UserId uid) = do
  logInfo_ $ "Transferring ownership of group " <> toText gid <> " to " <> toText uid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  sUser <- lookupUserById uid
  isMember <- runDB $ D.isUserInGroup (S._userId sUser) gid
  unless isMember $ throwError $ err400 {errBody = "New owner must be a member of the group"}
  runDB $ D.updateGroupOwner gid (S._userId sUser)
  getGroup (GroupId gid)

importGroupCSV :: AuthAdmin -> GroupId -> CurrencyCode -> Maybe ExchangeQuoteId -> Maybe DecimalRate -> CSVData -> MyHandler [Record]
importGroupCSV authAdmin (GroupId gid) currencyCode quote manual (CSVData csvData) = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  logInfo_ $ "Importing CSV into group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  (info, rateInput) <- resolveImportInput group currencyCode quote manual
  parseRecords info rateInput csvData >>= addExpenseRecords group

addExpenseRecord :: AuthAdmin -> GroupId -> ExpenseRecordRequest -> MyHandler Record
addExpenseRecord authAdmin (GroupId gid) req = do
  logInfo_ $ "Adding expense record to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  resolved <- resolveExpenseRequest group req
  (record, recordSplits) <- withGroupCurrencyLock group $ D.insertSplitRecord D.ExpenseKind gid Nothing resolved
  um <- getGroupUserMap gid
  currencies <- asks appCurrencies
  fromStoredData $ recordToSplitRecord D.ExpenseKind currencies um record recordSplits

addIncomeRecord :: AuthAdmin -> GroupId -> IncomeRecordRequest -> MyHandler Record
addIncomeRecord authAdmin (GroupId gid) req = do
  logInfo_ $ "Adding income record to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  resolved <- resolveIncomeRequest group req
  (record, recordSplits) <- withGroupCurrencyLock group $ D.insertSplitRecord D.IncomeKind gid Nothing resolved
  um <- getGroupUserMap gid
  currencies <- asks appCurrencies
  fromStoredData $ recordToSplitRecord D.IncomeKind currencies um record recordSplits

addTransferRecord :: AuthAdmin -> GroupId -> TransferRecordRequest -> MyHandler Record
addTransferRecord authAdmin (GroupId gid) req@TransferRecordRequest {..} = do
  logInfo_ $ "Adding transfer record to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  resolved <- resolveTransferRequest group req
  record <- withGroupCurrencyLock group $ D.insertTransferRecord gid resolved
  um <- getGroupUserMap gid
  currencies <- asks appCurrencies
  fromStoredData $ recordToTransferRecord currencies um (coerce toUserId) record

getGroupRecords :: AuthAdmin -> GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Record)
getGroupRecords authAdmin gid query offset limit sort = do
  logTrace_ $ "Fetching group records for admin: " <> authAdminUsername authAdmin
  getRecordPage gid query offset limit sort

getGroupRecord :: AuthAdmin -> GroupId -> RecordId -> MyHandler Record
getGroupRecord authAdmin (GroupId gid) (RecordId rid) = do
  logTrace_ $ "Fetching group record for admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  getAdminRecord gid rid

-- | Check all record IDs within this group before the atomic currency-locked deletion.
bulkDeleteRecords :: AuthAdmin -> GroupId -> BulkRequest RecordId -> MyHandler NoContent
bulkDeleteRecords authAdmin (GroupId gid) (BulkRequest recordIds) = do
  logInfo_ $ "Bulk deleting records in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  forM_ recordIds $ \(RecordId rid) -> ensureRecordExists gid rid
  (if null recordIds then runDBTransaction else withGroupCurrencyLock group) $
    forM_ recordIds $
      \(RecordId rid) -> D.deleteRecord gid rid
  pure NoContent

deleteRecord :: AuthAdmin -> GroupId -> RecordId -> MyHandler NoContent
deleteRecord authAdmin (GroupId gid) (RecordId rid) = do
  logInfo_ $ "Deleting record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  ensureRecordExists gid rid
  withGroupCurrencyLock group $ D.deleteRecord gid rid
  pure NoContent

updateTransfer :: AuthAdmin -> GroupId -> RecordId -> TransferRecordRequest -> MyHandler Record
updateTransfer authAdmin (GroupId gid) (RecordId rid) req = do
  logInfo_ $ "Updating transfer record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  old <- getAdminRecord gid rid
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  case old of
    TransferRecord {} -> do
      resolved <- resolveTransferRequest group req
      withGroupCurrencyLock group $ D.replaceTransferRecord gid rid resolved
      getAdminRecord gid rid
    _ -> throwError $ err400 {errBody = "Record is not a transfer record"}

updateExpense :: AuthAdmin -> GroupId -> RecordId -> ExpenseRecordRequest -> MyHandler Record
updateExpense authAdmin (GroupId gid) (RecordId rid) req@ExpenseRecordRequest {} = do
  logInfo_ $ "Updating expense record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  old <- getAdminRecord gid rid
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  case old of
    ExpenseRecord {} -> do
      resolved <- resolveExpenseRequest group req
      withGroupCurrencyLock group $ D.replaceSplitRecord D.ExpenseKind gid rid resolved
      getAdminRecord gid rid
    _ -> throwError $ err400 {errBody = "Record is not an expense record"}

updateIncome :: AuthAdmin -> GroupId -> RecordId -> IncomeRecordRequest -> MyHandler Record
updateIncome authAdmin (GroupId gid) (RecordId rid) req = do
  logInfo_ $ "Updating income record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  old <- getAdminRecord gid rid
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  case old of
    IncomeRecord {} -> do
      resolved <- resolveIncomeRequest group req
      withGroupCurrencyLock group $ D.replaceSplitRecord D.IncomeKind gid rid resolved
      getAdminRecord gid rid
    _ -> throwError $ err400 {errBody = "Record is not an income record"}

getGroupReport :: AuthAdmin -> GroupId -> MyHandler Report
getGroupReport authAdmin (GroupId gid) = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  logTrace_ $ "Fetching group report for admin: " <> authAdminUsername authAdmin
  getReportByGroupId gid

-- | Require an active uploader and validated expense participants even for operators.
-- Receipt and children commit together; empty receipts do not lock currency policy.
createReceipt :: AuthAdmin -> GroupId -> AdminCreateReceiptRequest -> MyHandler Receipt
createReceipt authAdmin (GroupId gid) AdminCreateReceiptRequest {..} = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  logInfo_ $ "Creating receipt in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  ensureActiveGroupUser gid uploadedByUserId
  resolved <- mapM (resolveExpenseRequest group) records
  (receipt, dbRecords) <- (if null resolved then runDBTransaction else withGroupCurrencyLock group) $ do
    rct <- D.insertReceipt gid (coerce uploadedByUserId) note
    let rctId = S._receiptId rct
    recs <- forM resolved $ D.insertSplitRecord D.ExpenseKind gid (Just rctId)
    pure (rct, recs)
  um <- getGroupUserMap gid
  currencies <- asks appCurrencies
  apiRecords <- fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um)) dbRecords
  let uploadedByApiUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure $ Receipt (coerce $ S._receiptId receipt) (coerce gid) uploadedByApiUser note apiRecords (S._receiptCreatedAt receipt)

getGroupReceipts :: AuthAdmin -> GroupId -> MyHandler [Receipt]
getGroupReceipts authAdmin (GroupId gid) = do
  logTrace_ $ "Fetching group receipts for admin: " <> authAdminUsername authAdmin
  getReceiptsByGroupId gid

bulkDeleteReceipts :: AuthAdmin -> GroupId -> BulkRequest ReceiptId -> MyHandler NoContent
bulkDeleteReceipts authAdmin (GroupId gid) (BulkRequest receiptIds) = do
  logInfo_ $ "Bulk deleting receipts in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  forM_ receiptIds $ \(ReceiptId rid) -> ensureReceiptExists gid rid
  children <- runDB $ concat <$> mapM (\(ReceiptId rid) -> D.getRecordsForReceipt gid rid) receiptIds
  (if null children then runDBTransaction else withGroupCurrencyLock group) $
    forM_ receiptIds $
      \(ReceiptId rid) -> D.deleteReceiptWithRecords gid rid
  pure NoContent

deleteReceipt :: AuthAdmin -> GroupId -> ReceiptId -> MyHandler NoContent
deleteReceipt authAdmin (GroupId gid) (ReceiptId rid) = do
  logInfo_ $ "Deleting receipt " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  ensureReceiptExists gid rid
  children <- runDB $ D.getRecordsForReceipt gid rid
  (if null children then runDBTransaction else withGroupCurrencyLock group) $
    D.deleteReceiptWithRecords gid rid
  pure NoContent

updateReceipt :: AuthAdmin -> GroupId -> ReceiptId -> UpdateReceiptRequest -> MyHandler Receipt
updateReceipt authAdmin (GroupId gid) (ReceiptId rid) UpdateReceiptRequest {..} = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  logInfo_ $ "Updating receipt " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  group <- fst <$> (fetchOrFail "Group" gid $ runDB (D.getGroupWithMembers gid))
  receipt <- fetchOrFail "Receipt" rid $ runDB (D.getReceipt gid rid)
  resolved <- mapM (resolveExpenseRequest group) records
  dbRecords <-
    (if null resolved then runDBTransaction else withGroupCurrencyLock group) $
      D.replaceReceiptRecords gid rid note resolved
  buildUpdatedReceipt gid rid note receipt dbRecords

-- | Return whether an inactive membership can be reused instead of inserting history anew.
resolveAddableGroupMember :: UUID -> UserId -> MyHandler (S.User, Bool)
resolveAddableGroupMember gid (UserId uid) = do
  sUser <- lookupUserById uid
  activeMember <- runDB $ D.isUserInGroup (S._userId sUser) gid
  when activeMember $ throwError $ err409 {errBody = "User is already a member of the group"}
  existingRow <- runDB $ D.isUserInGroupIncludingInactive (S._userId sUser) gid
  pure (sUser, existingRow)

-- | Resolve only active non-owner members, preserving historical rows on removal.
resolveRemovableGroupMember :: UUID -> UserId -> MyHandler UUID
resolveRemovableGroupMember gid (UserId uid) = do
  sUser <- lookupUserById uid
  let targetUid = S._userId sUser
  targetIsOwner <- runDB $ D.isGroupOwner targetUid gid
  when targetIsOwner $ throwError $ err400 {errBody = "Cannot deactivate the group owner; transfer ownership first"}
  isMember <- runDB $ D.isUserInGroup targetUid gid
  unless isMember $ throwError $ err404 {errBody = "User is not an active member of the group"}
  pure targetUid

ensureDistinctMembers :: [UserId] -> MyHandler ()
ensureDistinctMembers userIds = do
  when (Set.size (Set.fromList userIds) /= length userIds) $
    throwError $
      err400 {errBody = "Each member may appear only once"}

loadAllGroups :: MyHandler [Group]
loadAllGroups = do
  rows <- runDB D.getAllGroupWithMembers
  let groupIds = M.keys $ M.fromList [(S._groupId group, ()) | (group, _) <- rows]
  mapM (getGroup . coerce) groupIds

-- | Keep original uploader and creation time while projecting replacement expenses.
buildUpdatedReceipt :: UUID -> UUID -> Text -> S.Receipt -> [(S.Record, [S.RecordSplit])] -> MyHandler Receipt
buildUpdatedReceipt gid rid note receipt dbRecords = do
  um <- getGroupUserMap gid
  currencies <- asks appCurrencies
  apiRecords <- fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um)) dbRecords
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure $ Receipt (coerce rid) (coerce gid) uploadedByUser note apiRecords (S._receiptCreatedAt receipt)

-- | Scope the record lookup to its parent group and validate its persisted projection.
getAdminRecord :: UUID -> UUID -> MyHandler Record
getAdminRecord gid rid = do
  (record, ssplits) <- fetchOrFail "Record" rid $ runDB (D.getRecordWithSplits gid rid)
  um <- getGroupUserMap gid
  currencies <- asks appCurrencies
  fromStoredData $ recordToApiRecord currencies um record ssplits

filterGroups :: Maybe Text -> [Group] -> [Group]
filterGroups Nothing = id
filterGroups (Just queryText) = filter (matchesQuery queryText . groupNameOf)

sortGroups :: [Group] -> Maybe Text -> [Group]
sortGroups groups Nothing = sortOn groupNameOf groups
sortGroups groups (Just sortText) = applySort sortText groupNameOf groups

groupNameOf :: Group -> Text
groupNameOf Group {groupName = value} = value
