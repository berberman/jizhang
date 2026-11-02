{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Jizhang.API.Receipt.Source
  ( PreparedReceiptSource,
    prepareReceiptSource,
    persistReceiptSource,
    receiptImageByteLimit,
  )
where

import Control.Monad (unless)
import Control.Monad.Reader (asks, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.Beam.Postgres (Pg)
import Jizhang.API.App
import Jizhang.API.Internal.Error (fromInput)
import Jizhang.API.Types.ReceiptAI
import qualified Jizhang.API.Types.Record as Record
import qualified Jizhang.Database.Receipt as D
import qualified Jizhang.Database.ReceiptAI as AI
import Jizhang.ReceiptAI.Image
import Jizhang.ReceiptAI.Review (validateReceiptImportReview)
import Jizhang.RequestProtection (RequestProtectionConfig (protectionBulkBodyBytes), requestProtectionConfig)
import Servant

-- Keep never carries or rewrites historical bytes or review JSON.
data PreparedReceiptSource
  = PreparedKeep
  | PreparedRemove
  | PreparedReplace !ReceiptSourceInput !ValidatedReceiptImage

receiptImageByteLimit :: RequestProtectionConfig -> Int
receiptImageByteLimit configuration = fromInteger $ min (toInteger maxReceiptImageBytes) $
  max 0 (toInteger (protectionBulkBodyBytes configuration) - 256 * 1024) * 3 `div` 4

prepareReceiptSource :: UUID -> [Record.ExpenseRecordRequest] -> ReceiptSourceChange -> MyHandler PreparedReceiptSource
prepareReceiptSource _ _ KeepReceiptSource = pure PreparedKeep
prepareReceiptSource _ _ RemoveReceiptSource = pure PreparedRemove
prepareReceiptSource group records (ReplaceReceiptSource source) = do
  maximumBytes <- asks (receiptImageByteLimit . requestProtectionConfig . appRequestProtection)
  validated <- liftIO $ validateReceiptImage maximumBytes (receiptSourceImage source)
  image <- case validated of
    Left TooLarge -> throwError $ err413 {errBody = "Receipt image exceeds the server upload limit"}
    Left UnsupportedMedia -> throwError $ err415 {errBody = "Only normalized JPEG receipt images are supported"}
    Left InvalidImage -> throwError $ err400 {errBody = "Invalid receipt JPEG image"}
    Right value -> pure value
  case (receiptSourceExtractionId source, receiptSourceEvidence source, receiptSourceReview source) of
    (Nothing, Nothing, Nothing) -> pure ()
    (Just extraction, Just evidence, Just review) -> do
      catalog <- asks appCurrencies
      amounts <- fromInput $ validateReceiptImportReview catalog (Just evidence) review
      unless (length records == length amounts && and (zipWith (matches review) records amounts)) $
        throwError $ err400 {errBody = "Receipt expenses must match the confirmed import amounts and currency in review order"}
      let evidenceDigest = T.pack $ show (hash (canonicalReceiptEvidence evidence) :: Digest SHA256)
      accepted <- runDBPrivate $ AI.validateReceiptExtractionSource group extraction (validatedImageSha256 image) evidenceDigest
      unless accepted $ throwError $ err400 {errBody = "Receipt extraction does not match this group, image and original evidence"}
    _ -> throwError $ err400 {errBody = "Model source requires extractionId, evidence and review together"}
  pure (PreparedReplace source image)
  where
    matches review Record.ExpenseRecordRequest {Record.currency = currency, Record.amountMinor = originalAmount} (_, expectedAmount) =
      currency == reviewCurrency review && originalAmount == expectedAmount

persistReceiptSource :: UUID -> PreparedReceiptSource -> Pg ()
persistReceiptSource _ PreparedKeep = pure ()
persistReceiptSource receipt PreparedRemove = D.removeReceiptSource receipt
persistReceiptSource receipt (PreparedReplace source image) =
  D.putReceiptSource receipt source (validatedImageBytes image) (validatedImageWidth image) (validatedImageHeight image) (validatedImageSha256 image)
