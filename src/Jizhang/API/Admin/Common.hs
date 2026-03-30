{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.TypeLits (Symbol)
import Jizhang.API.Common (WithPagination)
import Jizhang.API.Types
import Jizhang.API.Utils (validateUsername)
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

type AdminGroupByIdAPI api =
  "admin" :> "groups" :> Capture "groupId" GroupId :> api

type AdminGroupMembersAPI api =
  AdminGroupByIdAPI ("members" :> api)

type AdminGroupRecordsAPI api =
  AdminGroupByIdAPI ("records" :> api)

type AdminGroupReceiptsAPI api =
  AdminGroupByIdAPI ("receipts" :> api)

paginatedListHandler :: Text -> MyHandler [src] -> (src -> dst) -> (Maybe Text -> [dst] -> [dst]) -> ([dst] -> Maybe Text -> [dst]) -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse dst)
paginatedListHandler logMsg loadItems toItem applyFilter sortFn mQuery mOffset mLimit mSort = do
  logInfo_ logMsg
  loadedItems <- loadItems
  let mapped = map toItem loadedItems
  pure $ paginateResponse mOffset mLimit mQuery mSort sortFn $ applyFilter mQuery mapped

bulkDeleteHandler :: Text -> [a] -> (a -> MyHandler ()) -> (a -> MyHandler ()) -> MyHandler NoContent
bulkDeleteHandler logMsg itemsToDelete validateOne deleteOne = do
  logInfo_ logMsg
  forM_ itemsToDelete validateOne
  forM_ itemsToDelete deleteOne
  pure NoContent

createWithPasswordHash :: Text -> Text -> Text -> MyHandler Bool -> BL8.ByteString -> (Text -> Text -> MyHandler entity) -> (entity -> result) -> MyHandler result
createWithPasswordHash logMsg targetUsername rawPassword existsAction existsErr insertAction toResult = do
  logInfo_ logMsg
  validateUsername targetUsername
  validatePasswordText rawPassword
  exists <- existsAction
  when exists $ throwError $ err409 {errBody = existsErr}
  passwordHash <- hashPasswordText rawPassword
  toResult <$> insertAction targetUsername passwordHash

validatePasswordText :: Text -> MyHandler ()
validatePasswordText rawPassword = do
  when (T.null rawPassword) $ throwError $ err400 {errBody = "Password cannot be empty"}
  when (T.length rawPassword < 6) $ throwError $ err400 {errBody = "Password must be at least 6 characters"}

hashPasswordText :: Text -> MyHandler Text
hashPasswordText rawPassword = do
  mHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 rawPassword)
  case mHash of
    Nothing -> throwError $ err500 {errBody = "Failed to hash password"}
    Just hash -> pure $ decodeUtf8 hash
