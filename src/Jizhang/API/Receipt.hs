{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Receipt where

import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import Data.UUID (toText)
import Jizhang.API.Types
import Jizhang.API.Utils
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

type ReceiptAPI =
  -- Create a new receipt with batch records
  "groups" :> Capture "groupId" GroupId :> "receipts" :> ReqBody '[JSON] CreateReceiptRequest :> Post '[JSON] Receipt
    -- List all receipts in a group
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> Get '[JSON] [Receipt]
    -- Get a specific receipt with its records
    :<|> "groups" :> Capture "groupId" GroupId :> "receipts" :> Capture "receiptId" ReceiptId :> Get '[JSON] Receipt
    -- Delete a receipt (records' receipt FK set to NULL)
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

createReceipt :: AuthUser -> GroupId -> CreateReceiptRequest -> MyHandler Receipt
createReceipt authUser (GroupId gId) CreateReceiptRequest {..} = do
  logInfo_ $ "Creating receipt in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  -- Validate all record requests upfront
  forM_ records $ validateExpenseRecordRequest gId
  -- Resolve all users before entering the DB transaction
  resolved <- forM records $ \ExpenseRecordRequest {..} -> do
    payerId <- S._userId <$> lookupUser (coerce byUsername)
    resolvedSplits <- forM splits $ \RecordSplitRequest {..} -> do
      uid <- S._userId <$> lookupUser (coerce username)
      pure (uid, share)
    pure (title, amount, payerId, date, resolvedSplits)
  -- Create receipt and all records in a single DB transaction
  (receipt, dbRecords) <- runDB $ do
    receipt <- D.insertReceipt gId (authUserId authUser) note
    let rctId = S._receiptId receipt
    recs <- forM resolved $ \(t, a, payerId, d, splitUsers) -> do
      rec <- D.insertRecord t a payerId Nothing gId d (Just rctId)
      ss <- forM splitUsers $ \(uid, sh) ->
        D.insertRecordSplit (S._recordId rec) uid sh
      pure (rec, ss)
    pure (receipt, recs)
  um <- getGroupUserMap gId
  let apiRecords = [recordToExpenseRecord um rec ss | (rec, ss) <- dbRecords]
      uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure
    Receipt
      { receiptId = coerce $ S._receiptId receipt,
        groupId = coerce gId,
        uploadedBy = uploadedByUser,
        note = S._receiptNote receipt,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt
      }

getReceipts :: AuthUser -> GroupId -> MyHandler [Receipt]
getReceipts authUser (GroupId gId) = do
  logInfo_ $ "Listing receipts in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  receipts <- runDB $ D.getReceiptsForGroup gId
  um <- getGroupUserMap gId
  forM receipts $ \receipt -> buildReceiptResponse um receipt

getReceipt :: AuthUser -> GroupId -> ReceiptId -> MyHandler Receipt
getReceipt authUser (GroupId gId) (ReceiptId rId) = do
  logInfo_ $ "Fetching receipt " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  receipt <- fetchOrFail "Receipt" rId $ runDB (D.getReceipt rId)
  um <- getGroupUserMap gId
  buildReceiptResponse um receipt

deleteReceipt :: AuthUser -> GroupId -> ReceiptId -> MyHandler NoContent
deleteReceipt authUser (GroupId gId) (ReceiptId rId) = do
  logInfo_ $ "Deleting receipt " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  ensureReceiptExists rId
  -- Delete associated records (with splits) and the receipt in one transaction
  runDB $ do
    recs <- D.getRecordsForReceipt rId
    forM_ recs $ \rec -> do
      D.deleteRecordSplitsForRecord (S._recordId rec)
      D.deleteRecord (S._recordId rec)
    D.deleteReceipt rId
  pure NoContent

updateReceipt :: AuthUser -> GroupId -> ReceiptId -> UpdateReceiptRequest -> MyHandler Receipt
updateReceipt authUser (GroupId gId) (ReceiptId rId) UpdateReceiptRequest {..} = do
  logInfo_ $ "Updating receipt " <> toText rId <> " in group " <> toText gId
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  receipt <- fetchOrFail "Receipt" rId $ runDB (D.getReceipt rId)
  -- Validate and resolve all users upfront
  forM_ records $ validateExpenseRecordRequest gId
  resolved <- forM records $ \ExpenseRecordRequest {..} -> do
    payerId <- S._userId <$> lookupUser (coerce byUsername)
    resolvedSplits <- forM splits $ \RecordSplitRequest {..} -> do
      uid <- S._userId <$> lookupUser (coerce username)
      pure (uid, share)
    pure (title, amount, payerId, date, resolvedSplits)
  -- Delete old records and insert new ones in a single DB transaction
  dbRecords <- runDB $ do
    D.updateReceiptNote rId note
    oldRecs <- D.getRecordsForReceipt rId
    forM_ oldRecs $ \rec -> do
      D.deleteRecordSplitsForRecord (S._recordId rec)
      D.deleteRecord (S._recordId rec)
    forM resolved $ \(t, a, payerId, d, splitUsers) -> do
      rec <- D.insertRecord t a payerId Nothing gId d (Just rId)
      ss <- forM splitUsers $ \(uid, sh) ->
        D.insertRecordSplit (S._recordId rec) uid sh
      pure (rec, ss)
  -- Build response
  um <- getGroupUserMap gId
  let apiRecords = [recordToExpenseRecord um rec ss | (rec, ss) <- dbRecords]
      uploadedByUser = resolveUser um (S.unUserId $ S._receiptUploadedBy receipt)
  pure
    Receipt
      { receiptId = coerce rId,
        groupId = coerce gId,
        uploadedBy = uploadedByUser,
        note = note,
        records = apiRecords,
        createdAt = S._receiptCreatedAt receipt
      }

-- | Build a Receipt API response from a schema Receipt
buildReceiptResponse :: UserMap -> S.Receipt -> MyHandler Receipt
buildReceiptResponse um receipt = do
  let rctId = S._receiptId receipt
  recs <- runDB $ D.getRecordsForReceipt rctId
  -- For each record, get its splits and build the API Record
  apiRecords <- forM recs $ \rec -> do
    if S.isTransferRecord rec
      then pure $ recordToTransferRecord um rec
      else do
        ss <- runDB $ D.getRecordSplitsForRecord (S._recordId rec)
        pure $ recordToExpenseRecord um rec ss
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
