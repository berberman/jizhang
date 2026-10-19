-- | Group-scoped receipt metadata and its attached expense records. Callers
-- authorize access and wrap multi-statement replacements/deletions in transactions.
module Jizhang.Database.Receipt where

import Control.Monad (forM, forM_)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import qualified Jizhang.Database.Record as R
import Jizhang.Database.Schema

-- | Create metadata only; no records or uploaded file contents are stored here.
-- The uploader's membership is a caller precondition.
insertReceipt :: UUID -> UUID -> Text -> Pg Receipt
insertReceipt groupId uploadedBy note = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let receipt = Receipt newUUID (GroupId groupId) (UserId uploadedBy) note now
  runInsert $ insert (_receipts jizhangDb) $ insertValues [receipt]
  pure receipt

getReceipt :: UUID -> UUID -> Pg (Maybe Receipt)
getReceipt groupId receiptId = runSelectReturningOne $ select $ do
  r <- all_ (_receipts jizhangDb)
  guard_ (_receiptGroup r ==. val_ (GroupId groupId) &&. _receiptId r ==. val_ receiptId)
  pure r

getReceiptsForGroup :: UUID -> Pg [Receipt]
getReceiptsForGroup groupId = runSelectReturningList $ select $ do
  r <- all_ (_receipts jizhangDb)
  guard_ (_receiptGroup r ==. val_ (GroupId groupId))
  pure r

updateReceiptNote :: UUID -> UUID -> Text -> Pg ()
updateReceiptNote groupId receiptId newNote =
  runUpdate $
    update
      (_receipts jizhangDb)
      (\r -> _receiptNote r <-. val_ newNote)
      (\r -> _receiptGroup r ==. val_ (GroupId groupId) &&. _receiptId r ==. val_ receiptId)

deleteReceipt :: UUID -> UUID -> Pg ()
deleteReceipt groupId receiptId = runDelete $ delete (_receipts jizhangDb) (\r -> _receiptGroup r ==. val_ (GroupId groupId) &&. _receiptId r ==. val_ receiptId)

-- | Select attachments by both group and receipt, without assuming that an
-- arbitrary receipt UUID supplied by a caller belongs to that group.
getRecordsForReceipt :: UUID -> UUID -> Pg [Record]
getRecordsForReceipt groupId receiptId = runSelectReturningList $ select $ do
  r <- all_ (_records jizhangDb)
  guard_ (_recordGroup r ==. val_ (GroupId groupId) &&. _recordReceipt r ==. val_ (just_ (ReceiptId receiptId)))
  pure r

-- | Remove attached records before metadata. Atomicity requires a surrounding
-- transaction; this helper does not acquire a receipt lock against concurrent edits.
deleteReceiptWithRecords :: UUID -> UUID -> Pg ()
deleteReceiptWithRecords groupId receiptId = do
  records <- getRecordsForReceipt groupId receiptId
  forM_ records $ \record -> R.deleteRecord groupId (_recordId record)
  deleteReceipt groupId receiptId

-- | Replace all attachments with newly identified expenses and update the note.
-- Missing receipts return []; callers validate allocations and serialize edits.
replaceReceiptRecords :: UUID -> UUID -> Text -> [R.ResolvedSplitRecord] -> Pg [(Record, [RecordSplit])]
replaceReceiptRecords groupId receiptId note resolved = do
  receipt <- getReceipt groupId receiptId
  case receipt of
    Nothing -> pure []
    Just _ -> do
      updateReceiptNote groupId receiptId note
      oldRecords <- getRecordsForReceipt groupId receiptId
      forM_ oldRecords $ \record -> R.deleteRecord groupId (_recordId record)
      forM resolved $ R.insertSplitRecord R.ExpenseKind groupId (Just receiptId)
