{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Jizhang.API.Admin.Groups
  ( getAllGroups,
    bulkDeleteGroups,
    getGroupById,
    updateGroup,
    deleteGroup,
    addGroupMember,
    bulkAddGroupMembers,
    deleteGroupMember,
    bulkDeleteGroupMembers,
    transferOwnership,
    getGroupRecords,
    bulkDeleteRecords,
    deleteRecord,
    updateTransfer,
    updateExpense,
    getGroupReport,
    getGroupReceipts,
    bulkDeleteReceipts,
    deleteReceipt,
    updateReceipt,
  )
where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Coerce (coerce)
import Data.Int (Int16)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time (Day)
import Data.UUID (UUID, toText)
import Jizhang.API.Admin.Common
import Jizhang.API.Group (getGroup)
import Jizhang.API.Receipt (getReceiptsByGroupId)
import Jizhang.API.Record (getRecordsByGroupId)
import Jizhang.API.Report (getReportByGroupId)
import Jizhang.API.Types
import Jizhang.API.Utils
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

bulkDeleteGroups :: AuthAdmin -> BulkRequest GroupId -> MyHandler NoContent
bulkDeleteGroups authAdmin (BulkRequest groupIds) =
  bulkDeleteHandler
    ("Bulk deleting groups as admin: " <> authAdminUsername authAdmin)
    groupIds
    (\(GroupId gid) -> ensureGroupExists gid)
    (\(GroupId gid) -> runDB $ D.deleteGroup gid)

getGroupById :: AuthAdmin -> GroupId -> MyHandler Group
getGroupById authAdmin gid = do
  logInfo_ $ "Fetching group for admin: " <> authAdminUsername authAdmin
  getGroup gid

updateGroup :: AuthAdmin -> GroupId -> Text -> MyHandler Group
updateGroup authAdmin (GroupId gid) newName = do
  logInfo_ $ "Updating group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  validateGroupName newName
  runDB $ D.updateGroup gid (Just newName)
  getGroup (GroupId gid)

deleteGroup :: AuthAdmin -> GroupId -> MyHandler NoContent
deleteGroup authAdmin (GroupId gid) = do
  logInfo_ $ "Deleting group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  runDB $ D.deleteGroup gid
  pure NoContent

addGroupMember :: AuthAdmin -> GroupId -> Username -> MyHandler Group
addGroupMember authAdmin (GroupId gid) (Username username) = do
  logInfo_ $ "Adding member " <> username <> " to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  (sUser, existingRow) <- resolveAddableGroupMember gid username
  if existingRow
    then runDB $ D.reactivateGroupMember (S._userId sUser) gid
    else void $ runDB $ D.addGroupMember (S._userId sUser) gid
  getGroup (GroupId gid)

bulkAddGroupMembers :: AuthAdmin -> GroupId -> BulkRequest Username -> MyHandler Group
bulkAddGroupMembers authAdmin (GroupId gid) (BulkRequest usernames) = do
  logInfo_ $ "Bulk adding members to group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  resolved <- forM usernames $ \(Username uname) -> resolveAddableGroupMember gid uname
  forM_ resolved $ \(sUser, existingRow) ->
    if existingRow
      then runDB $ D.reactivateGroupMember (S._userId sUser) gid
      else void $ runDB $ D.addGroupMember (S._userId sUser) gid
  getGroup (GroupId gid)

deleteGroupMember :: AuthAdmin -> GroupId -> Username -> MyHandler NoContent
deleteGroupMember authAdmin (GroupId gid) (Username username) = do
  logInfo_ $ "Removing member " <> username <> " from group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  targetUid <- resolveRemovableGroupMember gid username
  runDB $ D.deactivateGroupMember targetUid gid
  pure NoContent

bulkDeleteGroupMembers :: AuthAdmin -> GroupId -> BulkRequest Username -> MyHandler NoContent
bulkDeleteGroupMembers authAdmin (GroupId gid) (BulkRequest usernames) = do
  logInfo_ $ "Bulk removing members from group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  resolved <- forM usernames $ \(Username uname) -> resolveRemovableGroupMember gid uname
  forM_ resolved $ \uid -> runDB $ D.deactivateGroupMember uid gid
  pure NoContent

transferOwnership :: AuthAdmin -> GroupId -> Username -> MyHandler Group
transferOwnership authAdmin (GroupId gid) (Username username) = do
  logInfo_ $ "Transferring ownership of group " <> toText gid <> " to " <> username <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  sUser <- lookupUser username
  isMember <- runDB $ D.isUserInGroup (S._userId sUser) gid
  unless isMember $ throwError $ err400 {errBody = "New owner must be a member of the group"}
  runDB $ D.updateGroupOwner gid (S._userId sUser)
  getGroup (GroupId gid)

getGroupRecords :: AuthAdmin -> GroupId -> MyHandler [Record]
getGroupRecords authAdmin (GroupId gid) = do
  logInfo_ $ "Fetching group records for admin: " <> authAdminUsername authAdmin
  getRecordsByGroupId gid

bulkDeleteRecords :: AuthAdmin -> GroupId -> BulkRequest RecordId -> MyHandler NoContent
bulkDeleteRecords authAdmin (GroupId gid) (BulkRequest recordIds) = do
  ensureGroupExists gid
  bulkDeleteHandler
    ("Bulk deleting records in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin)
    recordIds
    (\(RecordId rid) -> ensureRecordExists rid)
    (\(RecordId rid) -> runDB $ D.deleteRecord rid)

deleteRecord :: AuthAdmin -> GroupId -> RecordId -> MyHandler NoContent
deleteRecord authAdmin (GroupId gid) (RecordId rid) = do
  logInfo_ $ "Deleting record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  ensureRecordExists rid
  runDB $ D.deleteRecord rid
  pure NoContent

updateTransfer :: AuthAdmin -> GroupId -> RecordId -> TransferRecordRequest -> MyHandler Record
updateTransfer authAdmin (GroupId gid) (RecordId rid) req@TransferRecordRequest {..} = do
  logInfo_ $ "Updating transfer record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  old <- getAdminRecord gid rid
  case old of
    TransferRecord {} -> do
      validateTransferRecordRequest gid req
      payer <- lookupUser (coerce byUsername)
      receiver <- lookupUser (coerce toUsername)
      runDB $ D.updateRecord rid Nothing (Just amount) (Just $ S._userId payer) (Just $ Just $ S._userId receiver) (Just date)
      getAdminRecord gid rid
    _ -> throwError $ err400 {errBody = "Record is not a transfer record"}

updateExpense :: AuthAdmin -> GroupId -> RecordId -> ExpenseRecordRequest -> MyHandler Record
updateExpense authAdmin (GroupId gid) (RecordId rid) req@ExpenseRecordRequest {} = do
  logInfo_ $ "Updating expense record " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  old <- getAdminRecord gid rid
  case old of
    ExpenseRecord existingRid _ _ _ _ _ _ _ -> do
      resolved <- resolveExpenseUpdate gid req
      replaceExpenseRecord rid existingRid resolved
      getAdminRecord gid rid
    _ -> throwError $ err400 {errBody = "Record is not an expense record"}

getGroupReport :: AuthAdmin -> GroupId -> MyHandler Report
getGroupReport authAdmin (GroupId gid) = do
  logInfo_ $ "Fetching group report for admin: " <> authAdminUsername authAdmin
  getReportByGroupId gid

getGroupReceipts :: AuthAdmin -> GroupId -> MyHandler [Receipt]
getGroupReceipts authAdmin (GroupId gid) = do
  logInfo_ $ "Fetching group receipts for admin: " <> authAdminUsername authAdmin
  getReceiptsByGroupId gid

bulkDeleteReceipts :: AuthAdmin -> GroupId -> BulkRequest ReceiptId -> MyHandler NoContent
bulkDeleteReceipts authAdmin (GroupId gid) (BulkRequest receiptIds) = do
  ensureGroupExists gid
  bulkDeleteHandler
    ("Bulk deleting receipts in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin)
    receiptIds
    (\(ReceiptId rid) -> ensureReceiptExists rid)
    (\(ReceiptId rid) -> deleteReceiptCascade rid)

deleteReceipt :: AuthAdmin -> GroupId -> ReceiptId -> MyHandler NoContent
deleteReceipt authAdmin (GroupId gid) (ReceiptId rid) = do
  logInfo_ $ "Deleting receipt " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  ensureReceiptExists rid
  deleteReceiptCascade rid
  pure NoContent

updateReceipt :: AuthAdmin -> GroupId -> ReceiptId -> UpdateReceiptRequest -> MyHandler Receipt
updateReceipt authAdmin (GroupId gid) (ReceiptId rid) UpdateReceiptRequest {..} = do
  logInfo_ $ "Updating receipt " <> toText rid <> " in group " <> toText gid <> " as admin: " <> authAdminUsername authAdmin
  ensureGroupExists gid
  receipt <- fetchOrFail "Receipt" rid $ runDB (D.getReceipt rid)
  resolved <- resolveReceiptUpdate gid records
  dbRecords <- replaceReceiptRecords gid rid note resolved
  buildUpdatedReceipt gid rid note receipt dbRecords

resolveAddableGroupMember :: UUID -> Text -> MyHandler (S.User, Bool)
resolveAddableGroupMember gid username = do
  sUser <- lookupUser username
  activeMember <- runDB $ D.isUserInGroup (S._userId sUser) gid
  when activeMember $ throwError $ err409 {errBody = "User is already a member of the group"}
  existingRow <- runDB $ D.isUserInGroupIncludingInactive (S._userId sUser) gid
  pure (sUser, existingRow)

resolveRemovableGroupMember :: UUID -> Text -> MyHandler UUID
resolveRemovableGroupMember gid username = do
  sUser <- lookupUser username
  let targetUid = S._userId sUser
  targetIsOwner <- runDB $ D.isGroupOwner targetUid gid
  when targetIsOwner $ throwError $ err400 {errBody = "Cannot deactivate the group owner; transfer ownership first"}
  isMember <- runDB $ D.isUserInGroup targetUid gid
  unless isMember $ throwError $ err404 {errBody = "User is not an active member of the group"}
  pure targetUid

loadAllGroups :: MyHandler [Group]
loadAllGroups = do
  rows <- runDB D.getAllGroupWithMembers
  let groupIds = M.keys $ M.fromList [(S._groupId group, ()) | (group, _) <- rows]
  mapM (getGroup . coerce) groupIds

resolveExpenseUpdate :: UUID -> ExpenseRecordRequest -> MyHandler (Text, Double, UUID, Day, [(UUID, Int16)])
resolveExpenseUpdate gid req@ExpenseRecordRequest {..} = do
  validateExpenseRecordRequest gid req
  payer <- lookupUser (coerce byUsername)
  splitUsers <- forM splits $ \RecordSplitRequest {..} -> do
    u <- lookupUser (coerce username)
    pure (S._userId u, share)
  pure (title, amount, S._userId payer, date, splitUsers)

replaceExpenseRecord :: UUID -> RecordId -> (Text, Double, UUID, Day, [(UUID, Int16)]) -> MyHandler ()
replaceExpenseRecord rid existingRid (recordTitle, recordAmount, payerId, recordDate, splitUsers) =
  runDB $ do
    D.deleteRecordSplitsForRecord rid
    D.updateRecord rid (Just recordTitle) (Just recordAmount) (Just payerId) Nothing (Just recordDate)
    forM_ splitUsers $ \(uid, sh) -> D.insertRecordSplit (coerce existingRid) uid sh

resolveReceiptUpdate :: UUID -> [ExpenseRecordRequest] -> MyHandler [(Text, Double, UUID, Day, [(UUID, Int16)])]
resolveReceiptUpdate gid = mapM (resolveExpenseUpdate gid)

replaceReceiptRecords :: UUID -> UUID -> Text -> [(Text, Double, UUID, Day, [(UUID, Int16)])] -> MyHandler [(S.Record, [S.RecordSplit])]
replaceReceiptRecords gid rid note resolved =
  runDB $ do
    D.updateReceiptNote rid note
    oldRecs <- D.getRecordsForReceipt rid
    forM_ oldRecs $ \rec -> do
      D.deleteRecordSplitsForRecord (S._recordId rec)
      D.deleteRecord (S._recordId rec)
    forM resolved $ \(recordTitle, recordAmount, payerId, recordDate, splitUsers) -> do
      rec <- D.insertRecord recordTitle recordAmount payerId Nothing gid recordDate (Just rid)
      ss <- forM splitUsers $ \(uid, sh) -> D.insertRecordSplit (S._recordId rec) uid sh
      pure (rec, ss)

buildUpdatedReceipt :: UUID -> UUID -> Text -> S.Receipt -> [(S.Record, [S.RecordSplit])] -> MyHandler Receipt
buildUpdatedReceipt gid rid note receipt dbRecords = do
  um <- getGroupUserMap gid
  let apiRecords = [recordToExpenseRecord um rec ss | (rec, ss) <- dbRecords]
      uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure $ Receipt (coerce rid) (coerce gid) uploadedByUser note apiRecords (S._receiptCreatedAt receipt)

getAdminRecord :: UUID -> UUID -> MyHandler Record
getAdminRecord gid rid = do
  (record, ssplits) <- fetchOrFail "Record" rid $ runDB (D.getRecordWithSplits rid)
  um <- getGroupUserMap gid
  pure $ if S.isTransferRecord record then recordToTransferRecord um record else recordToExpenseRecord um record ssplits

deleteReceiptCascade :: UUID -> MyHandler ()
deleteReceiptCascade rid = runDB $ do
  recs <- D.getRecordsForReceipt rid
  forM_ recs $ \rec -> do
    D.deleteRecordSplitsForRecord (S._recordId rec)
    D.deleteRecord (S._recordId rec)
  D.deleteReceipt rid

filterGroups :: Maybe Text -> [Group] -> [Group]
filterGroups Nothing = id
filterGroups (Just queryText) = filter (matchesQuery queryText . groupName)

sortGroups :: [Group] -> Maybe Text -> [Group]
sortGroups groups Nothing = sortOn groupName groups
sortGroups groups (Just sortText) = applySort sortText groupName groups
