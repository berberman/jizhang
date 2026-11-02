{-# LANGUAGE OverloadedStrings #-}

-- | Group-scoped receipt metadata and its attached expense records. Callers
-- authorize access and wrap multi-statement replacements/deletions in transactions.
module Jizhang.Database.Receipt where

import Control.Monad (forM, forM_)
import Data.Aeson (FromJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList, runUpdateReturningList)
import Database.Beam.Postgres
import Jizhang.API.Types.ReceiptAI (ReceiptImageInput (..), ReceiptSourceInput (..), ReceiptSourceMeta (..), canonicalReceiptEvidence)
import qualified Jizhang.Database.Record as R
import Jizhang.Database.Schema

-- | Create metadata only; no records or uploaded file contents are stored here.
-- The uploader's membership is a caller precondition.
insertReceipt :: UUID -> UUID -> Text -> Text -> Pg Receipt
insertReceipt groupId uploadedBy title note = do
  newUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let receipt = Receipt newUUID (GroupId groupId) (UserId uploadedBy) title note now
  inserted <- runInsertReturningList $ insert (_receipts jizhangDb) $ insertValues [receipt]
  case inserted of
    [storedReceipt] -> pure storedReceipt
    _ -> liftIO $ ioError $ userError "Expected one inserted receipt"

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

getReceiptTitlesForGroup :: UUID -> Pg [(UUID, Text)]
getReceiptTitlesForGroup groupId = runSelectReturningList $ select $ do
  receipt <- all_ (_receipts jizhangDb)
  guard_ (_receiptGroup receipt ==. val_ (GroupId groupId))
  pure (_receiptId receipt, _receiptTitle receipt)

updateReceiptMetadata :: UUID -> UUID -> Text -> Text -> Pg ()
updateReceiptMetadata groupId receiptId newTitle newNote =
  runUpdate $
    update
      (_receipts jizhangDb)
      ( \r ->
          mconcat
            [ _receiptTitle r <-. val_ newTitle,
              _receiptNote r <-. val_ newNote
            ]
      )
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

-- | Replace all attachments with newly identified expenses and update metadata.
-- Missing receipts return []; callers validate allocations and serialize edits.
replaceReceiptRecords :: UUID -> UUID -> Text -> Text -> [R.ResolvedSplitRecord] -> Pg [(Record, [RecordSplit])]
replaceReceiptRecords groupId receiptId title note resolved = do
  receipt <- getReceipt groupId receiptId
  case receipt of
    Nothing -> pure []
    Just _ -> do
      updateReceiptMetadata groupId receiptId title note
      oldRecords <- getRecordsForReceipt groupId receiptId
      forM_ oldRecords $ \record -> R.deleteRecord groupId (_recordId record)
      forM resolved $ R.insertSplitRecord R.ExpenseKind groupId (Just receiptId)

-- The no-op update acquires a row lock without fetching source bytes.
lockReceipt :: UUID -> UUID -> Pg (Maybe Receipt)
lockReceipt groupId receiptId = do
  rows <-
    runUpdateReturningList $
      update
        (_receipts jizhangDb)
        (\row -> _receiptId row <-. val_ receiptId)
        (\row -> _receiptGroup row ==. val_ (GroupId groupId) &&. _receiptId row ==. val_ receiptId)
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing

getReceiptSourceMeta :: UUID -> UUID -> Pg (Maybe ReceiptSourceMeta)
getReceiptSourceMeta groupId receiptId = do
  row <- runSelectReturningOne $ select $ do
    receipt <- all_ (_receipts jizhangDb)
    guard_ (_receiptId receipt ==. val_ receiptId &&. _receiptGroup receipt ==. val_ (GroupId groupId))
    source <- related_ (_receiptSources jizhangDb) (ReceiptSourceId $ primaryKey receipt)
    pure
      ( _receiptSourceMediaType source,
        _receiptSourceWidth source,
        _receiptSourceHeight source,
        _receiptSourceByteCount source,
        _receiptSourceSha256 source,
        isJust_ (_receiptSourceReviewJson source)
      )
  pure $
    fmap
      ( \(mediaType, width, height, byteCount, digest, hasReview) ->
          ReceiptSourceMeta mediaType (fromIntegral width) (fromIntegral height) (fromIntegral byteCount) digest hasReview
      )
      row

getReceiptSource :: UUID -> UUID -> Pg (Maybe ReceiptSourceInput)
getReceiptSource groupId receiptId = do
  row <- runSelectReturningOne $ select $ do
    receipt <- all_ (_receipts jizhangDb)
    guard_ (_receiptId receipt ==. val_ receiptId &&. _receiptGroup receipt ==. val_ (GroupId groupId))
    related_ (_receiptSources jizhangDb) (ReceiptSourceId $ primaryKey receipt)
  traverse decodeSource row
  where
    decodeSource source = do
      evidence <- traverse decodeStoredJson (_receiptSourceEvidenceJson source)
      review <- traverse decodeStoredJson (_receiptSourceReviewJson source)
      pure $
        ReceiptSourceInput
          (ReceiptImageInput (_receiptSourceMediaType source) (decodeUtf8 $ B64.encode $ _receiptSourceImageBytes source))
          (unReceiptAiUsageId $ _receiptSourceExtraction source)
          evidence
          review

decodeStoredJson :: (FromJSON a) => Text -> Pg a
decodeStoredJson jsonText = case eitherDecodeStrict' (encodeUtf8 jsonText) of
  Right value -> pure value
  Left _ -> liftIO $ ioError $ userError "Private data persistence failed"

-- Callers validate the image and provenance, then hold the parent receipt lock
-- for replacements. New receipts and their source share the creating transaction.
putReceiptSource :: UUID -> ReceiptSourceInput -> BS.ByteString -> Int -> Int -> Text -> Pg ()
putReceiptSource receiptId source bytes width height digest = do
  let row =
        ReceiptSource
          (ReceiptId receiptId)
          (receiptImageMediaType $ receiptSourceImage source)
          bytes
          (fromIntegral width)
          (fromIntegral height)
          (fromIntegral $ BS.length bytes)
          digest
          (ReceiptAiUsageId $ receiptSourceExtractionId source)
          (decodeUtf8 . canonicalReceiptEvidence <$> receiptSourceEvidence source)
          (decodeUtf8 . LBS.toStrict . encode <$> receiptSourceReview source)
  existing <- runSelectReturningOne $ select $ do
    stored <- all_ (_receiptSources jizhangDb)
    guard_ (_receiptSourceReceipt stored ==. val_ (ReceiptId receiptId))
    pure (unReceiptId $ _receiptSourceReceipt stored)
  case existing of
    Nothing -> runInsert $ insert (_receiptSources jizhangDb) (insertValues [row])
    Just _ -> runUpdate $ save (_receiptSources jizhangDb) row

removeReceiptSource :: UUID -> Pg ()
removeReceiptSource receiptId =
  runDelete $
    delete
      (_receiptSources jizhangDb)
      (\source -> _receiptSourceReceipt source ==. val_ (ReceiptId receiptId))
