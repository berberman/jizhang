{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Jizhang.API.Utils where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Char8 as LBS
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Jizhang.API.Types
import Jizhang.Common.MyUUID
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Servant

textToLBS :: Text -> LazyByteString
textToLBS = LBS.fromStrict . T.encodeUtf8

ensureUserExists :: Text -> MyHandler ()
ensureUserExists u = do
  exists <- runDB $ D.checkUserExists u
  unless exists $ throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}

ensureGroupExists :: MyUUID -> MyHandler ()
ensureGroupExists gId = do
  exists <- runDB $ D.checkGroupExists gId
  unless exists $ throwError $ err404 {errBody = "Group with ID " <> textToLBS (uuidToText gId) <> " not found"}

validateUsername :: Text -> MyHandler ()
validateUsername u = do
  when (T.null u) $ throwError $ err400 {errBody = "Username cannot be empty"}
  when (T.length u > 50) $ throwError $ err400 {errBody = "Username is too long"}

validateGroupName :: Text -> MyHandler ()
validateGroupName gname = do
  when (T.null gname) $ throwError $ err400 {errBody = "Group name cannot be empty"}
  when (T.length gname > 100) $ throwError $ err400 {errBody = "Group name is too long"}

validateExpenseRecordRequest :: ExpenseRecordRequest -> MyHandler ()
validateExpenseRecordRequest ExpenseRecordRequest {..} = do
  when (T.null title) $ throwError $ err400 {errBody = "Title cannot be empty"}
  when (amount <= 0) $ throwError $ err400 {errBody = "Amount must be greater than zero"}
  ensureUserExists (coerce byUsername)
  forM_ splits $ \RecordSplitRequest {..} -> do
    ensureUserExists (coerce username)
    when (share < 0) $
      throwError $
        err400 {errBody = "Share must be non-negative"}

validateTransferRecordRequest :: TransferRecordRequest -> MyHandler ()
validateTransferRecordRequest TransferRecordRequest {..} = do
  when (amount <= 0) $ throwError $ err400 {errBody = "Amount must be greater than zero"}
  ensureUserExists (coerce byUsername)
  ensureUserExists (coerce toUsername)
  when (byUsername == toUsername) $
    throwError $
      err400 {errBody = "Transfer cannot be made to the same user"}

recordToExpenseRecord :: S.Record -> [S.RecordSplit] -> Record
recordToExpenseRecord record ssplits =
  let recordId = coerce $ S._recordId record
      title = S._title record
      amount = S._amount record
      byUsername = coerce . S.unUserId $ S._paidBy record
      date = S._date record
      groupId = coerce . S.unGroupId $ S._recordGroup record
      splits = [RecordSplit (coerce $ S.unUserId _rsUser) _share _splitAmount | S.RecordSplit {..} <- ssplits]
   in ExpenseRecord {..}

recordToTransferRecord :: S.Record -> Record
recordToTransferRecord record =
  let recordId = coerce $ S._recordId record
      title = S._title record
      amount = S._amount record
      byUsername = coerce . S.unUserId $ S._paidBy record
      toUsername = coerce . fromJust . S.unUserId $ S._transferTo record
      date = S._date record
      groupId = coerce . S.unGroupId $ S._recordGroup record
   in TransferRecord {..}

ensureRecordExists :: MyUUID -> MyHandler ()
ensureRecordExists rId = do
  exists <- runDB $ D.checkRecordExists rId
  unless exists $ throwError $ err404 {errBody = "Record with ID " <> textToLBS (uuidToText rId) <> " not found"}
