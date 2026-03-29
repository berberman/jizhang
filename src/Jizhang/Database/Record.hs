module Jizhang.Database.Record where

import Data.Int (Int16)
import Data.Text (Text)
import Data.Time (Day, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import Jizhang.Database.Schema

insertRecord :: Text -> Double -> UUID -> Maybe UUID -> UUID -> Day -> Maybe UUID -> Pg Record
insertRecord title amount paidBy transferTo groupId recordTime receiptId = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let record =
        Record
          { _recordId = newUUID,
            _recordGroup = GroupId groupId,
            _title = title,
            _amount = amount,
            _paidBy = UserId paidBy,
            _transferTo = maybe nothing_ (just_ . UserId) transferTo,
            _date = recordTime,
            _createdAt = now,
            _recordReceipt = maybe nothing_ (just_ . ReceiptId) receiptId
          }
  runInsert $ insert (_records jizhangDb) $ insertValues [record]
  pure record

deleteRecord :: UUID -> Pg ()
deleteRecord recordId = runDelete $ delete (_records jizhangDb) (\record -> _recordId record ==. val_ recordId)

updateRecord :: UUID -> Maybe Text -> Maybe Double -> Maybe UUID -> Maybe (Maybe UUID) -> Maybe Day -> Pg ()
updateRecord recordId title amount paidBy transferTo recordDate = do
  runUpdate $
    update
      (_records jizhangDb)
      ( \record ->
          mconcat $
            [_title record <-. val_ x | Just x <- [title]]
              <> [_amount record <-. val_ x | Just x <- [amount]]
              <> [_paidBy record <-. val_ (UserId x) | Just x <- [paidBy]]
              <> [_transferTo record <-. val_ (maybe nothing_ (just_ . UserId) x) | Just x <- [transferTo]]
              <> [_date record <-. val_ x | Just x <- [recordDate]]
      )
      (\record -> _recordId record ==. val_ recordId)

checkRecordExists :: UUID -> Pg Bool
checkRecordExists recordId = do
  records <- runSelectReturningList $ select $ do
    record <- all_ (_records jizhangDb)
    guard_ (_recordId record ==. val_ recordId)
    pure record
  pure $ not (null records)

insertRecordSplit :: UUID -> UUID -> Int16 -> Pg RecordSplit
insertRecordSplit recordId userId share = do
  let split = RecordSplit (RecordId recordId) (UserId userId) share
  runInsert (insert (_recordSplits jizhangDb) $ insertValues [split]) >> pure split

deleteRecordSplitsForRecord :: UUID -> Pg ()
deleteRecordSplitsForRecord recordId = runDelete $ delete (_recordSplits jizhangDb) (\rs -> _rsRecord rs ==. val_ (RecordId recordId))

updateRecordSplit :: UUID -> UUID -> Maybe Int16 -> Pg ()
updateRecordSplit recordId userId share = do
  runUpdate $
    update
      (_recordSplits jizhangDb)
      ( \rs ->
          mconcat $
            [_share rs <-. val_ x | Just x <- [share]]
      )
      (\rs -> _rsRecord rs ==. val_ (RecordId recordId) &&. _rsUser rs ==. val_ (UserId userId))

getRecordWithSplits :: UUID -> Pg (Maybe (Record, [RecordSplit]))
getRecordWithSplits recordId = do
  record <- runSelectReturningOne $ select $ do
    r <- all_ (_records jizhangDb)
    guard_ (_recordId r ==. val_ recordId)
    pure r
  case record of
    Just rec -> do
      splits <- getRecordSplitsForRecord recordId
      pure $ Just (rec, splits)
    Nothing -> pure Nothing

getRecordSplitsForRecord :: UUID -> Pg [RecordSplit]
getRecordSplitsForRecord recordId = runSelectReturningList $ select $ do
  rs <- all_ (_recordSplits jizhangDb)
  guard_ (_rsRecord rs ==. val_ (RecordId recordId))
  pure rs

getRecordsWithSplitsForGroup :: UUID -> Pg [(Record, Maybe RecordSplit)]
getRecordsWithSplitsForGroup groupId = runSelectReturningList $ select $ do
  record <- all_ (_records jizhangDb)
  guard_ (_recordGroup record ==. val_ (GroupId groupId))
  splits <- leftJoin_ (all_ (_recordSplits jizhangDb)) (\rs -> _rsRecord rs ==. primaryKey record)
  pure (record, splits)
