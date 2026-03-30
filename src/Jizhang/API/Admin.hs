{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Admin
  ( AdminAPI,
    adminServer,
  )
where

import Data.Text (Text)
import Jizhang.API.Admin.Common
import Jizhang.API.Admin.Groups
import Jizhang.API.Admin.Users
import Jizhang.API.GroupImport (CSV, CSVData)
import Jizhang.API.Types
import Servant

type AdminAPI =
  AdminPaginatedGetAPI "users" User
    :<|> AdminCreateAPI "users" AdminCreateUserRequest User
    :<|> AdminBulkPostNamedAPI "users" "bulk-delete" UserId NoContent
    :<|> AdminDeleteByIdAPI "users" "userId" UserId
    :<|> AdminPaginatedGetAPI "admins" AdminSummary
    :<|> AdminCreateAPI "admins" AdminCreateAdminRequest AdminSummary
    :<|> AdminPaginatedGetAPI "groups" Group
    :<|> AdminCreateAPI "groups" AdminCreateGroupRequest Group
    :<|> AdminBulkPostNamedAPI "groups" "bulk-delete" GroupId NoContent
    :<|> AdminGroupByIdAPI (Get '[JSON] Group)
    :<|> AdminGroupByIdAPI (ReqBody '[JSON] Text :> Put '[JSON] Group)
    :<|> AdminGroupByIdAPI (Delete '[JSON] NoContent)
    :<|> AdminGroupMembersAPI (ReqBody '[JSON] Username :> Post '[JSON] Group)
    :<|> AdminGroupMembersAPI ("bulk-add" :> ReqBody '[JSON] (BulkRequest Username) :> Post '[JSON] Group)
    :<|> AdminGroupMembersAPI (Capture "username" Username :> Delete '[JSON] NoContent)
    :<|> AdminGroupMembersAPI ("bulk-delete" :> ReqBody '[JSON] (BulkRequest Username) :> Post '[JSON] NoContent)
    :<|> AdminGroupByIdAPI ("owner" :> ReqBody '[JSON] Username :> Put '[JSON] Group)
    :<|> AdminGroupByIdAPI ("import" :> ReqBody '[CSV] CSVData :> Post '[JSON] [Record])
    :<|> AdminGroupRecordsAPI ("expense" :> ReqBody '[JSON] ExpenseRecordRequest :> Post '[JSON] Record)
    :<|> AdminGroupRecordsAPI ("transfer" :> ReqBody '[JSON] TransferRecordRequest :> Post '[JSON] Record)
    :<|> AdminGroupRecordsAPI (Get '[JSON] [Record])
    :<|> AdminGroupRecordsAPI ("bulk-delete" :> ReqBody '[JSON] (BulkRequest RecordId) :> Post '[JSON] NoContent)
    :<|> AdminGroupRecordsAPI (Capture "recordId" RecordId :> Delete '[JSON] NoContent)
    :<|> AdminGroupRecordsAPI ("transfer" :> Capture "recordId" RecordId :> ReqBody '[JSON] TransferRecordRequest :> Put '[JSON] Record)
    :<|> AdminGroupRecordsAPI ("expense" :> Capture "recordId" RecordId :> ReqBody '[JSON] ExpenseRecordRequest :> Put '[JSON] Record)
    :<|> AdminGroupByIdAPI ("report" :> Get '[JSON] Report)
    :<|> AdminGroupReceiptsAPI (ReqBody '[JSON] AdminCreateReceiptRequest :> Post '[JSON] Receipt)
    :<|> AdminGroupReceiptsAPI (Get '[JSON] [Receipt])
    :<|> AdminGroupReceiptsAPI ("bulk-delete" :> ReqBody '[JSON] (BulkRequest ReceiptId) :> Post '[JSON] NoContent)
    :<|> AdminGroupReceiptsAPI (Capture "receiptId" ReceiptId :> Delete '[JSON] NoContent)
    :<|> AdminGroupReceiptsAPI (Capture "receiptId" ReceiptId :> ReqBody '[JSON] UpdateReceiptRequest :> Put '[JSON] Receipt)

adminServer :: AuthAdmin -> MyServer AdminAPI
adminServer authAdmin =
  getAllUsers authAdmin
    :<|> createUser authAdmin
    :<|> bulkDeleteUsers authAdmin
    :<|> deleteUser authAdmin
    :<|> getAllAdmins authAdmin
    :<|> createAdmin authAdmin
    :<|> getAllGroups authAdmin
    :<|> createGroup authAdmin
    :<|> bulkDeleteGroups authAdmin
    :<|> getGroupById authAdmin
    :<|> updateGroup authAdmin
    :<|> deleteGroup authAdmin
    :<|> addGroupMember authAdmin
    :<|> bulkAddGroupMembers authAdmin
    :<|> deleteGroupMember authAdmin
    :<|> bulkDeleteGroupMembers authAdmin
    :<|> transferOwnership authAdmin
    :<|> importGroupCSV authAdmin
    :<|> addExpenseRecord authAdmin
    :<|> addTransferRecord authAdmin
    :<|> getGroupRecords authAdmin
    :<|> bulkDeleteRecords authAdmin
    :<|> deleteRecord authAdmin
    :<|> updateTransfer authAdmin
    :<|> updateExpense authAdmin
    :<|> getGroupReport authAdmin
    :<|> createReceipt authAdmin
    :<|> getGroupReceipts authAdmin
    :<|> bulkDeleteReceipts authAdmin
    :<|> deleteReceipt authAdmin
    :<|> updateReceipt authAdmin
