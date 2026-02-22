module Jizhang.Database.Receipt where

import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import Jizhang.Database.Schema

insertReceipt :: UUID -> UUID -> Text -> Pg Receipt
insertReceipt groupId uploadedBy note = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let receipt = Receipt newUUID (GroupId groupId) (UserId uploadedBy) note now
  runInsert $ insert (_receipts jizhangDb) $ insertValues [receipt]
  pure receipt

getReceipt :: UUID -> Pg (Maybe Receipt)
getReceipt receiptId = runSelectReturningOne $ select $ do
  r <- all_ (_receipts jizhangDb)
  guard_ (_receiptId r ==. val_ receiptId)
  pure r

getReceiptsForGroup :: UUID -> Pg [Receipt]
getReceiptsForGroup groupId = runSelectReturningList $ select $ do
  r <- all_ (_receipts jizhangDb)
  guard_ (_receiptGroup r ==. val_ (GroupId groupId))
  pure r

updateReceiptNote :: UUID -> Text -> Pg ()
updateReceiptNote receiptId newNote =
  runUpdate $
    update
      (_receipts jizhangDb)
      (\r -> _receiptNote r <-. val_ newNote)
      (\r -> _receiptId r ==. val_ receiptId)

deleteReceipt :: UUID -> Pg ()
deleteReceipt receiptId = runDelete $ delete (_receipts jizhangDb) (\r -> _receiptId r ==. val_ receiptId)

getRecordsForReceipt :: UUID -> Pg [Record]
getRecordsForReceipt receiptId = runSelectReturningList $ select $ do
  r <- all_ (_records jizhangDb)
  guard_ (_recordReceipt r ==. val_ (just_ (ReceiptId receiptId)))
  pure r
