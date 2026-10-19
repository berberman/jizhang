{-# LANGUAGE OverloadedStrings #-}

-- | Ledger operations shared by member and operator handlers. Callers supply
-- authorization; this layer owns projection, query semantics and atomic imports.
module Jizhang.API.Record.Service
  ( getRecordsByGroupId,
    projectRecordRows,
    getRecordPage,
    addExpenseRecords,
  )
where

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.UUID (UUID)
import Jizhang.API.App
import Jizhang.API.Internal.Error (fromInput, fromStoredData)
import Jizhang.API.Internal.Resource (ensureGroupExists, getGroupUserMap)
import Jizhang.API.Record.Input (resolveExpenseRequest, withGroupCurrencyLock)
import Jizhang.API.Record.Projection (recordToApiRecord, recordToSplitRecord)
import Jizhang.API.Types (ExpenseRecordRequest, GroupId (GroupId), PaginatedResponse, Record, paginateResponse)
import Jizhang.API.User.Projection (UserMap)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.RecordQuery as RQ

-- | Load a complete group ledger; callers must authorize the group first.
getRecordsByGroupId :: UUID -> MyHandler [Record]
getRecordsByGroupId gId = do
  ensureGroupExists gId
  rs <- runDB $ D.getRecordsWithSplitsForGroup gId
  um <- getGroupUserMap gId
  projectRecordRows um rs

-- | Collapse joined parents once and validate persisted monetary allocations.
projectRecordRows :: UserMap -> [(S.Record, Maybe S.RecordSplit)] -> MyHandler [Record]
projectRecordRows um rs = do
  let mp = M.fromListWith (++) [(r, maybeToList s) | (r, s) <- rs]
  currencies <- asks appCurrencies
  fromStoredData $
    traverse (\(r, ss) -> recordToApiRecord currencies um r ss) $
      sortOn (S._createdAt . fst) $
        M.toList mp

-- | Parse the query and sort the entire matching ledger before selecting a page.
getRecordPage :: GroupId -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Record)
getRecordPage (GroupId gId) mQuery mOffset mLimit mSort = do
  currencies <- asks appCurrencies
  records <- getRecordsByGroupId gId
  query <- fromInput $ RQ.parseRecordQuery currencies (maybe mempty id mQuery)
  ordering <- fromInput $ RQ.parseRecordSort mSort
  let effectiveSort = Just $ maybe "-date" id mSort
  pure $
    paginateResponse mOffset mLimit mQuery effectiveSort (\xs _ -> RQ.sortRecords ordering xs) $
      filter (RQ.matchesRecordQuery query) records

-- | Validate every row before the atomic import; empty imports do not lock currency.
addExpenseRecords :: S.Group -> [ExpenseRecordRequest] -> MyHandler [Record]
addExpenseRecords group requests = do
  let gId = S._groupId group
  resolved <- mapM (resolveExpenseRequest group) requests
  dbRecords <-
    (if null resolved then runDBTransaction else withGroupCurrencyLock group) $
      forM resolved $
        D.insertSplitRecord D.ExpenseKind gId Nothing
  um <- getGroupUserMap gId
  currencies <- asks appCurrencies
  fromStoredData $ traverse (uncurry (recordToSplitRecord D.ExpenseKind currencies um)) dbRecords
