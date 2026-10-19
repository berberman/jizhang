{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Operator route composition. The enclosing authenticated API supplies the admin
-- principal; group handlers bypass member authorization but retain resource scoping.
module Jizhang.API.Admin
  ( AdminAPI,
    adminServer,
  )
where

import Jizhang.API.Admin.Authentication
import Jizhang.API.Admin.Common
import Jizhang.API.Admin.Groups
import Jizhang.API.Admin.Users
import Jizhang.API.Common (WithPagination)
import Jizhang.API.ExchangeRate (createAdminExchangeQuote)
import Jizhang.API.GroupImport (CSV, CSVData)
import Jizhang.API.Statistics (getAdminStatistics)
import Jizhang.API.App
import Jizhang.API.Types
import qualified Jizhang.API.Types.ExchangeRate as Q
import qualified Jizhang.API.Types.OIDC as Oidc
import Servant

-- | Full /admin paths include bulk mutations and account authentication management.
-- Bulk requests carry explicit IDs rather than applying to the current filtered page.
type AdminAPI =
  AdminPaginatedGetAPI "users" Oidc.AdminUserSummary
    :<|> AdminCreateAPI "users" AdminCreateUserRequest Oidc.AdminUserDetails
    :<|> AdminBulkPostNamedAPI "users" "bulk-delete" UserId NoContent
    :<|> AdminDeleteByIdAPI "users" "userId" UserId
    :<|> AdminPaginatedGetAPI "admins" AdminSummary
    :<|> AdminCreateAPI "admins" AdminCreateAdminRequest AdminSummary
    :<|> AdminPaginatedGetAPI "groups" Group
    :<|> AdminCreateAPI "groups" AdminCreateGroupRequest Group
    :<|> AdminBulkPostNamedAPI "groups" "bulk-delete" GroupId NoContent
    :<|> AdminGroupByIdAPI (Get '[JSON] Group)
    :<|> AdminGroupByIdAPI (ReqBody '[JSON] UpdateGroupRequest :> Put '[JSON] Group)
    :<|> AdminGroupByIdAPI (Delete '[JSON] NoContent)
    :<|> AdminGroupMembersAPI (ReqBody '[JSON] UserId :> Post '[JSON] Group)
    :<|> AdminGroupMembersAPI ("bulk-add" :> ReqBody '[JSON] (BulkRequest UserId) :> Post '[JSON] Group)
    :<|> AdminGroupMembersAPI (Capture "userId" UserId :> Delete '[JSON] NoContent)
    :<|> AdminGroupMembersAPI ("bulk-delete" :> ReqBody '[JSON] (BulkRequest UserId) :> Post '[JSON] NoContent)
    :<|> AdminGroupByIdAPI ("owner" :> ReqBody '[JSON] UserId :> Put '[JSON] Group)
    :<|> AdminGroupByIdAPI ("import" :> QueryParam' '[Required, Strict] "currency" CurrencyCode :> QueryParam "exchangeQuoteId" ExchangeQuoteId :> QueryParam "manualRate" DecimalRate :> ReqBody '[CSV] CSVData :> Post '[JSON] [Record])
    :<|> AdminGroupRecordsAPI ("expense" :> ReqBody '[JSON] ExpenseRecordRequest :> Post '[JSON] Record)
    :<|> AdminGroupRecordsAPI ("transfer" :> ReqBody '[JSON] TransferRecordRequest :> Post '[JSON] Record)
    :<|> AdminGroupRecordsAPI (WithPagination (Get '[JSON] (PaginatedResponse Record)))
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
    :<|> AdminGroupByIdAPI ("exchange-quotes" :> ReqBody '[JSON] Q.ExchangeQuoteRequest :> Post '[JSON] Q.ExchangeQuote)
    :<|> AdminGroupByIdAPI StatisticsEndpoint
    :<|> AdminGroupRecordsAPI ("income" :> ReqBody '[JSON] IncomeRecordRequest :> Post '[JSON] Record)
    :<|> AdminGroupRecordsAPI ("income" :> Capture "recordId" RecordId :> ReqBody '[JSON] IncomeRecordRequest :> Put '[JSON] Record)
    :<|> AdminGroupRecordsAPI (Capture "recordId" RecordId :> Get '[JSON] Record)
    :<|> (AdminUserManagementAPI :<|> AdminAuthenticationAPI)

-- | Bind one authenticated operator to every branch in the route declaration's order.
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
    :<|> createAdminExchangeQuote authAdmin
    :<|> getAdminStatistics authAdmin
    :<|> addIncomeRecord authAdmin
    :<|> updateIncome authAdmin
    :<|> getGroupRecord authAdmin
    :<|> (adminUserManagementServer authAdmin :<|> adminAuthenticationServer authAdmin)
