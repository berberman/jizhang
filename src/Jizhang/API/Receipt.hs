{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Member-authorized receipt routes group expense records under one uploader.
-- Batch mutations validate child requests before atomically changing the receipt.
module Jizhang.API.Receipt where

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.List (sortOn)
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Jizhang.API.Common
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (UserPrincipal))
import Jizhang.API.Types
import Jizhang.API.Internal.Error (fromStoredData, fetchOrFail)
import Jizhang.API.User.Projection (UserMap, resolveUser)
import Jizhang.API.Record.Projection (recordToSplitRecord)
import Jizhang.API.Record.Input (resolveExpenseRequest, withGroupCurrencyLock)
import Jizhang.API.Internal.Resource (getGroupUserMap, ensureGroupExists, ensureGroupMember, ensureReceiptExists)
import Jizhang.Currency (CurrencyCatalog)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

-- | Receipt IDs are scoped to the captured group; replacement replaces all children,
-- and deletion removes the receipt together with its child expenses.
type ReceiptAPI =
  -- Create a new receipt with batch records
  "groups" :> Capture "groupId" GroupId :> "receipts" :> ReqBody '[JSON] CreateReceiptRequest :> Post '[JSON] Receipt
    -- List all receipts in a group
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> WithPagination (Get '[JSON] (PaginatedResponse Receipt))
    -- Get a specific receipt with its records
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> Capture "receiptId" ReceiptId :> Get '[JSON] Receipt
    -- Delete a receipt and its child expenses
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> Capture "receiptId" ReceiptId :> Delete '[JSON] NoContent
    -- Update a receipt (replace note and all records)
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> Capture "receiptId" ReceiptId :> ReqBody '[JSON] UpdateReceiptRequest :> Put '[JSON] Receipt

receiptServer :: AuthUser -> MyServer ReceiptAPI
receiptServer authUser =
  createReceipt authUser
    :<|> getReceipts authUser
    :<|> getReceipt authUser
    :<|> deleteReceipt authUser
    :<|> updateReceipt authUser

-- | Shared loader without authorization; callers must authorize the parent group.
getReceiptsByGroupId :: UUID -> MyHandler [Receipt]
getReceiptsByGroupId gId = do
  ensureGroupExists gId
  receipts <- runDB $ D.getReceiptsForGroup gId
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  forM receipts $ \receipt -> buildReceiptResponse currencies um receipt

-- | Insert the receipt and all validated expenses together; an empty receipt does
-- not lock group currency policy. The authenticated member becomes its uploader.
createReceipt :: AuthUser -> GroupId -> CreateReceiptRequest -> MyHandler Receipt
createReceipt authUser (GroupId gId) CreateReceiptRequest {..} = do
  enforceExpensiveLimit (UserPrincipal $ authUserId authUser)
  logInfo_ $ "Creating receipt in group " <> toText gId
  group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
  ensureGroupMember (authUserId authUser) gId
  resolved <- mapM (resolveExpenseRequest group) records
  (receipt, dbRecords) <- (if null resolved then runDBTransaction else withGroupCurrencyLock group) $ do
    receipt <- D.insertReceipt gId (authUserId authUser) note
    let rctId = S._receiptId receipt
    recs <- forM resolved $ D.insertSplitRecord D.ExpenseKind gId (Just rctId)
    pure (receipt, recs)
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  apiRecords <- fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um)) dbRecords
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure
    Receipt
      { receiptId = coerce $ S._receiptId receipt,
        groupId = coerce gId,
        uploadedBy = uploadedByUser,
        note = S._receiptNote receipt,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt
      }

getReceipts :: AuthUser -> GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Receipt)
getReceipts authUser (GroupId gId) mQuery mOffset mLimit mSort = do
  logInfo_ $ "Listing receipts in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  receipts <- getReceiptsByGroupId gId
  pure $ paginateResponse mOffset mLimit mQuery mSort sortReceipts $ filterReceipts mQuery receipts

getReceipt :: AuthUser -> GroupId -> ReceiptId -> MyHandler Receipt
getReceipt authUser (GroupId gId) (ReceiptId rId) = do
  logInfo_ $ "Fetching receipt " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  receipt <- fetchOrFail "Receipt" rId $ runDB (D.getReceipt gId rId)
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  buildReceiptResponse currencies um receipt

deleteReceipt :: AuthUser -> GroupId -> ReceiptId -> MyHandler NoContent
deleteReceipt authUser (GroupId gId) (ReceiptId rId) = do
  logInfo_ $ "Deleting receipt " <> toText rId <> " in group " <> toText gId
  group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
  ensureGroupMember (authUserId authUser) gId
  ensureReceiptExists gId rId
  children <- runDB $ D.getRecordsForReceipt gId rId
  (if null children then runDBTransaction else withGroupCurrencyLock group) $
    D.deleteReceiptWithRecords gId rId
  pure NoContent

-- | Replace the note and children while retaining the original uploader and creation time.
updateReceipt :: AuthUser -> GroupId -> ReceiptId -> UpdateReceiptRequest -> MyHandler Receipt
updateReceipt authUser (GroupId gId) (ReceiptId rId) UpdateReceiptRequest {..} = do
  enforceExpensiveLimit (UserPrincipal $ authUserId authUser)
  logInfo_ $ "Updating receipt " <> toText rId <> " in group " <> toText gId
  group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
  ensureGroupMember (authUserId authUser) gId
  receipt <- fetchOrFail "Receipt" rId $ runDB (D.getReceipt gId rId)
  resolved <- mapM (resolveExpenseRequest group) records
  dbRecords <-
    (if null resolved then runDBTransaction else withGroupCurrencyLock group) $
      D.replaceReceiptRecords gId rId note resolved
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  apiRecords <- fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um)) dbRecords
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure
    Receipt
      { receiptId = coerce rId,
        groupId = coerce gId,
        uploadedBy = uploadedByUser,
        note = note,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt
      }

-- | Project persisted child allocations; the historical user map must contain the uploader.
buildReceiptResponse :: CurrencyCatalog -> UserMap -> S.Receipt -> MyHandler Receipt
buildReceiptResponse currencies um receipt = do
  let rctId = S._receiptId receipt
      gId = S.unGroupId $ S._receiptGroup receipt
  recs <- runDB $ D.getRecordsForReceipt gId rctId
  apiRecords <- forM recs $ \rec -> do
    ss <- runDB $ D.getRecordSplitsForRecord (S._recordId rec)
    fromStoredData $ recordToSplitRecord D.ExpenseKind currencies um rec ss
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure
    Receipt
      { receiptId = coerce rctId,
        groupId = coerce . S.unGroupId $ S._receiptGroup receipt,
        uploadedBy = uploadedByUser,
        note = S._receiptNote receipt,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt
      }

filterReceipts :: Maybe Text -> [Receipt] -> [Receipt]
filterReceipts Nothing = id
filterReceipts (Just queryText) = filter (matchesQuery queryText . receiptNoteOf)

sortReceipts :: [Receipt] -> Maybe Text -> [Receipt]
sortReceipts receipts Nothing = sortOn receiptNoteOf receipts
sortReceipts receipts (Just sortText) = applySort sortText receiptNoteOf receipts

receiptNoteOf :: Receipt -> Text
receiptNoteOf (Receipt _ _ _ note _ _) = note
