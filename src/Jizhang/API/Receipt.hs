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
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Jizhang.API.Common
import Jizhang.API.App
import Jizhang.API.Receipt.Source (prepareReceiptSource, persistReceiptSource)
import Jizhang.API.ReceiptAI (ReceiptExtractionAPI, receiptExtractionServer)
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (UserPrincipal, AdminPrincipal))
import Jizhang.API.Types
import Jizhang.API.Internal.Error (fromStoredData, fetchOrFail)
import Jizhang.API.User.Projection (UserMap, resolveUser)
import Jizhang.API.Record.Projection (recordToSplitRecord)
import Jizhang.API.Record.Input (resolveExpenseRequest, withGroupCurrencyLockPrivate)
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
    -- Update a receipt (replace title, note and all records)
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> Capture "receiptId" ReceiptId :> ReqBody '[JSON] UpdateReceiptRequest :> Put '[JSON] Receipt
    :<|> ("groups" :> Capture "groupId" GroupId :> "receipts" :> Capture "receiptId" ReceiptId :> "source" :> Get '[JSON] (Headers '[Header "Cache-Control" Text] ReceiptSourceInput)
      :<|> ReceiptExtractionAPI)

receiptServer :: AuthUser -> MyServer ReceiptAPI
receiptServer authUser =
  createReceipt authUser
    :<|> getReceipts authUser
    :<|> getReceipt authUser
    :<|> deleteReceipt authUser
    :<|> updateReceipt authUser
    :<|> (getReceiptSource authUser :<|> receiptExtractionServer authUser)

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
  preparedSource <- prepareReceiptSource gId records (maybe KeepReceiptSource ReplaceReceiptSource source)
  (receipt, dbRecords) <- (if null resolved then runDBPrivateTransaction else withGroupCurrencyLockPrivate group) $ do
    receipt <- D.insertReceipt gId (authUserId authUser) title note
    let rctId = S._receiptId receipt
    recs <- forM resolved $ D.insertSplitRecord D.ExpenseKind gId (Just rctId)
    persistReceiptSource rctId preparedSource
    pure (receipt, recs)
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  let summaries = M.singleton (S._receiptId receipt) (ReceiptSummary (coerce $ S._receiptId receipt) (S._receiptTitle receipt))
  apiRecords <- fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um summaries)) dbRecords
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  sourceMeta <- runDBPrivate $ D.getReceiptSourceMeta gId (S._receiptId receipt)
  pure
    Receipt
      { receiptId = coerce $ S._receiptId receipt,
        groupId = coerce gId,
        uploadedBy = uploadedByUser,
        title = S._receiptTitle receipt,
        note = S._receiptNote receipt,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt,
        source = sourceMeta
      }

getReceipts :: AuthUser -> GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Receipt)
getReceipts authUser (GroupId gId) mQuery mOffset mLimit mSort = do
  logTrace_ $ "Listing receipts in group " <> toText gId
  ensureGroupMember (authUserId authUser) gId
  receipts <- getReceiptsByGroupId gId
  pure $ paginateResponse mOffset mLimit mQuery mSort sortReceipts $ filterReceipts mQuery receipts

getReceipt :: AuthUser -> GroupId -> ReceiptId -> MyHandler Receipt
getReceipt authUser (GroupId gId) (ReceiptId rId) = do
  logTrace_ $ "Fetching receipt " <> toText rId <> " in group " <> toText gId
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
  (if null children then runDBPrivateTransaction else withGroupCurrencyLockPrivate group) $
    D.deleteReceiptWithRecords gId rId
  pure NoContent

-- | Replace metadata and children while retaining the original uploader and creation time.
updateReceipt :: AuthUser -> GroupId -> ReceiptId -> UpdateReceiptRequest -> MyHandler Receipt
updateReceipt authUser (GroupId gId) (ReceiptId rId) UpdateReceiptRequest {..} = do
  enforceExpensiveLimit (UserPrincipal $ authUserId authUser)
  logInfo_ $ "Updating receipt " <> toText rId <> " in group " <> toText gId
  group <- fst <$> (fetchOrFail "Group" gId $ runDB (D.getGroupWithMembers gId))
  ensureGroupMember (authUserId authUser) gId
  resolved <- mapM (resolveExpenseRequest group) records
  preparedSource <- prepareReceiptSource gId records source
  (receipt, dbRecords) <- fetchOrFail "Receipt" rId $
    (if null resolved then runDBPrivateTransaction else withGroupCurrencyLockPrivate group) $ do
      locked <- D.lockReceipt gId rId
      case locked of
        Nothing -> pure Nothing
        Just receipt -> do
          children <- D.replaceReceiptRecords gId rId title note resolved
          persistReceiptSource rId preparedSource
          pure $ Just (receipt, children)
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  let summaries = M.singleton rId (ReceiptSummary (coerce rId) title)
  apiRecords <- fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um summaries)) dbRecords
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  sourceMeta <- runDBPrivate $ D.getReceiptSourceMeta gId rId
  pure
    Receipt
      { receiptId = coerce rId,
        groupId = coerce gId,
        uploadedBy = uploadedByUser,
        title = title,
        note = note,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt,
        source = sourceMeta
      }

-- | Project persisted child allocations; the historical user map must contain the uploader.
buildReceiptResponse :: CurrencyCatalog -> UserMap -> S.Receipt -> MyHandler Receipt
buildReceiptResponse currencies um receipt = do
  let rctId = S._receiptId receipt
      gId = S.unGroupId $ S._receiptGroup receipt
      summaries = M.singleton rctId (ReceiptSummary (coerce rctId) (S._receiptTitle receipt))
  recs <- runDB $ D.getRecordsForReceipt gId rctId
  apiRecords <- forM recs $ \rec -> do
    ss <- runDB $ D.getRecordSplitsForRecord (S._recordId rec)
    fromStoredData $ recordToSplitRecord D.ExpenseKind currencies um summaries rec ss
  let uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  sourceMeta <- runDBPrivate $ D.getReceiptSourceMeta gId rctId
  pure
    Receipt
      { receiptId = coerce rctId,
        groupId = coerce . S.unGroupId $ S._receiptGroup receipt,
        uploadedBy = uploadedByUser,
        title = S._receiptTitle receipt,
        note = S._receiptNote receipt,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt,
        source = sourceMeta
      }

filterReceipts :: Maybe Text -> [Receipt] -> [Receipt]
filterReceipts Nothing = id
filterReceipts (Just queryText) = filter $ \receipt@(Receipt _ _ _ _ note _ _ _) ->
  matchesQuery queryText (receiptTitleOf receipt) || matchesQuery queryText note

sortReceipts :: [Receipt] -> Maybe Text -> [Receipt]
sortReceipts receipts Nothing = sortOn receiptTitleOf receipts
sortReceipts receipts (Just sortText) = applySort sortText receiptTitleOf receipts

receiptTitleOf :: Receipt -> Text
receiptTitleOf (Receipt _ _ _ title _ _ _ _) = title

getReceiptSource :: AuthUser -> GroupId -> ReceiptId -> MyHandler (Headers '[Header "Cache-Control" Text] ReceiptSourceInput)
getReceiptSource authUser (GroupId gid) (ReceiptId rid) = do
  ensureGroupMember (authUserId authUser) gid
  enforceExpensiveLimit (UserPrincipal $ authUserId authUser)
  source <- fetchOrFail "Receipt source" rid $ runDBPrivate (D.getReceiptSource gid rid)
  pure $ addHeader "no-store" source

getAdminReceiptSource :: AuthAdmin -> GroupId -> ReceiptId -> MyHandler (Headers '[Header "Cache-Control" Text] ReceiptSourceInput)
getAdminReceiptSource authAdmin (GroupId gid) (ReceiptId rid) = do
  ensureGroupExists gid
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  source <- fetchOrFail "Receipt source" rid $ runDBPrivate (D.getReceiptSource gid rid)
  pure $ addHeader "no-store" source
