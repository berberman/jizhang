{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Current-account routes bind the target UUID to the authenticated user, never a
-- request handle. Group lists project historical users but load the caller's groups.
module Jizhang.API.User where

import Control.Monad (forM, when)
import Control.Monad.Reader (asks)
import Data.List (sortOn)
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Jizhang.API.AccountAuthentication (AccountAuthenticationAPI, accountAuthenticationServer)
import Jizhang.API.Common
import Jizhang.API.App
import Jizhang.API.Types
import Jizhang.API.Internal.Error (textToLBS, fromStoredData)
import Jizhang.API.User.Account (normalizeUsername, normalizeDisplayName)
import Jizhang.API.User.Projection (schemaUserToProfile)
import Jizhang.API.Group.Projection (groupToApiGroup)
import Jizhang.API.Internal.Resource (lookupUserById, getGroupUserMap)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

-- | Self-service profile and membership views plus account authentication management.
type UserAPI =
  "users"
    :> "me"
    :> Get '[JSON] UserProfile
    :<|> "users"
      :> "me"
      :> Delete '[JSON] NoContent
    :<|> "users"
      :> "me"
      :> "groups"
      :> WithPagination (Get '[JSON] (PaginatedResponse Group))
    :<|> "users" :> "me" :> ReqBody '[JSON] UpdateProfileRequest :> Put '[JSON] UserProfile
    :<|> AccountAuthenticationAPI

userServer :: AuthUser -> MyServer UserAPI
userServer authUser =
  getMe authUser
    :<|> deleteMe authUser
    :<|> getMyGroups authUser
    :<|> updateMe authUser
    :<|> accountAuthenticationServer authUser

getMe :: AuthUser -> MyHandler UserProfile
getMe AuthUser {..} = do
  logTrace_ $ "Fetching current user: " <> toText authUserId
  sUser <- lookupUserById authUserId
  pure $ schemaUserToProfile sUser

-- | Normalize public labels, reject another account's handle and update/read atomically.
updateMe :: AuthUser -> UpdateProfileRequest -> MyHandler UserProfile
updateMe AuthUser {..} (UpdateProfileRequest rawUsername rawDisplayName) = do
  _ <- lookupUserById authUserId
  newUsername <- normalizeUsername rawUsername
  newDisplayName <- normalizeDisplayName rawDisplayName
  existing <- runDB $ D.getUserByUsername newUsername
  when (maybe False ((/= authUserId) . S._userId) existing) $
    throwError $
      err409 {errBody = "Username is already in use"}
  updated <- runDBTransaction $ do
    D.updateUserProfile authUserId newUsername newDisplayName
    D.getUserById authUserId
  case updated of
    Just value -> pure $ schemaUserToProfile value
    Nothing -> throwError $ err404 {errBody = "User with ID " <> textToLBS (toText authUserId) <> " not found"}

deleteMe :: AuthUser -> MyHandler NoContent
deleteMe AuthUser {..} = do
  logInfo_ $ "Deleting current user: " <> toText authUserId
  runDB $ D.deleteUser authUserId
  pure NoContent

getMyGroups :: AuthUser -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse Group)
getMyGroups AuthUser {..} mQuery mOffset mLimit mSort = do
  logTrace_ $ "Fetching groups for current user: " <> toText authUserId
  groups <- getMyGroupsForUser authUserId
  pure $ paginateResponse mOffset mLimit mQuery mSort sortGroups $ filterGroups mQuery groups

-- | Load groups for this UUID; callers must bind or authorize the requested account.
getMyGroupsForUser :: UUID -> MyHandler [Group]
getMyGroupsForUser authUserId = do
  groups <- runDB $ D.getGroupsForUser authUserId
  currencies <- asks appCurrencies
  forM groups $ \g -> do
    let gid = S._groupId g
    um <- getGroupUserMap gid
    ms <- runDB $ D.getAllMembersInGroup gid
    fromStoredData $ groupToApiGroup currencies um g ms

filterGroups :: Maybe Text -> [Group] -> [Group]
filterGroups Nothing = id
filterGroups (Just queryText) = filter (matchesQuery queryText . groupNameOf)

sortGroups :: [Group] -> Maybe Text -> [Group]
sortGroups groups Nothing = sortOn groupNameOf groups
sortGroups groups (Just sortText) = applySort sortText groupNameOf groups

groupNameOf :: Group -> Text
groupNameOf Group {groupName = value} = value
