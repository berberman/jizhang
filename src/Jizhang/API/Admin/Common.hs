{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Operator route aliases and shared collection/mutation plumbing. These helpers
-- do not authenticate callers; the enclosing operator API supplies that boundary.
module Jizhang.API.Admin.Common
  ( AdminPaginatedGetAPI,
    AdminCreateAPI,
    AdminBulkPostAPI,
    AdminBulkPostNamedAPI,
    AdminDeleteByIdAPI,
    AdminGroupByIdAPI,
    AdminGroupMembersAPI,
    AdminGroupRecordsAPI,
    AdminGroupReceiptsAPI,
    paginatedListHandler,
    bulkDeleteHandler,
    createWithPasswordHash,
    matchesQuery,
    applySort,
  )
where

import Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Database.Beam.Postgres (Pg)
import GHC.TypeLits (Symbol)
import Jizhang.API.Common (WithPagination)
import Jizhang.API.App
import Jizhang.API.Types
import Jizhang.API.User.Account (validateUsername, validatePasswordText, hashPasswordText)
import Log.Class
import Servant

type AdminPaginatedGetAPI (path :: Symbol) a =
  "admin" :> path :> WithPagination (Get '[JSON] (PaginatedResponse a))

type AdminCreateAPI (path :: Symbol) req res =
  "admin" :> path :> ReqBody '[JSON] req :> Post '[JSON] res

type AdminBulkPostAPI (path :: Symbol) item res =
  "admin" :> path :> ReqBody '[JSON] (BulkRequest item) :> Post '[JSON] res

type AdminBulkPostNamedAPI (path :: Symbol) (name :: Symbol) item res =
  "admin" :> path :> name :> ReqBody '[JSON] (BulkRequest item) :> Post '[JSON] res

type AdminDeleteByIdAPI (path :: Symbol) captureName captureType =
  "admin" :> path :> Capture captureName captureType :> Delete '[JSON] NoContent

-- | Capture a parent group once so nested member, record and receipt IDs stay scoped.
type AdminGroupByIdAPI api =
  "admin" :> "groups" :> Capture "groupId" GroupId :> api

type AdminGroupMembersAPI api =
  AdminGroupByIdAPI ("members" :> api)

type AdminGroupRecordsAPI api =
  AdminGroupByIdAPI ("records" :> api)

type AdminGroupReceiptsAPI api =
  AdminGroupByIdAPI ("receipts" :> api)

-- | Project and filter the full collection before sorting and selecting the page.
paginatedListHandler :: Text -> MyHandler [src] -> (src -> dst) -> (Maybe Text -> [dst] -> [dst]) -> ([dst] -> Maybe Text -> [dst]) -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse dst)
paginatedListHandler logMsg loadItems toItem applyFilter sortFn mQuery mOffset mLimit mSort = do
  logTrace_ logMsg
  loadedItems <- loadItems
  let mapped = map toItem loadedItems
  pure $ paginateResponse mOffset mLimit mQuery mSort sortFn $ applyFilter mQuery mapped

-- | Validate every target before the delete transaction; validation itself is not
-- part of that transaction, so database constraints still arbitrate concurrent changes.
bulkDeleteHandler :: Text -> [a] -> (a -> MyHandler ()) -> (a -> Pg ()) -> MyHandler NoContent
bulkDeleteHandler logMsg itemsToDelete validateOne deleteOne = do
  logInfo_ logMsg
  forM_ itemsToDelete validateOne
  runDBTransaction $ forM_ itemsToDelete deleteOne
  pure NoContent

-- | Validate and hash before invoking the caller's insert action; that action owns
-- transaction choice and any database uniqueness conflict handling.
createWithPasswordHash :: Text -> Text -> Text -> MyHandler Bool -> BL8.ByteString -> (Text -> Text -> MyHandler entity) -> (entity -> result) -> MyHandler result
createWithPasswordHash logMsg targetUsername rawPassword existsAction existsErr insertAction toResult = do
  logInfo_ logMsg
  validateUsername targetUsername
  validatePasswordText rawPassword
  exists <- existsAction
  when exists $ throwError $ err409 {errBody = existsErr}
  passwordHash <- hashPasswordText rawPassword
  toResult <$> insertAction targetUsername passwordHash
