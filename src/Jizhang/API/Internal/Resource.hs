{-# LANGUAGE OverloadedStrings #-}

-- | Shared existence, membership and ownership guards for group-scoped handlers.
-- Existence checks do not authorize access, and separate calls are not a transaction.
module Jizhang.API.Internal.Resource
  ( ensureUserExists,
    lookupUser,
    lookupUserById,
    getGroupUserMap,
    matchesUserQuery,
    ensureGroupExists,
    ensureGroupOwner,
    ensureGroupMember,
    validateGroupName,
    ensureActiveGroupUser,
    ensureRecordExists,
    ensureReceiptExists
  )
where

import Control.Monad (unless, when)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, toText)
import Jizhang.API.App (MyHandler, runDB, runDBSensitive)
import Jizhang.API.Internal.Error (textToLBS)
import Jizhang.API.Types.Common (matchesQuery)
import Jizhang.API.Types.User (User (User), UserId (UserId))
import Jizhang.API.User.Projection (UserMap, schemaUserToApiUser)
import qualified Jizhang.Database as D (checkGroupExists, checkRecordExists, checkUserExists, getReceipt, getUserById, getUserByUsername, getUsersForGroup, isGroupOwner, isUserInGroup)
import qualified Jizhang.Database.Schema as S
import Servant (ServerError (errBody), err400, err403, err404, throwError)

ensureUserExists :: Text -> MyHandler ()
ensureUserExists u = do
  exists <- runDB $ D.checkUserExists u
  unless exists $ throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}

-- | Load a full account without SQL parameter logging; absence is a 404.
lookupUser :: Text -> MyHandler S.User
lookupUser u = do
  mUser <- runDBSensitive $ D.getUserByUsername u
  case mUser of
    Just user -> pure user
    Nothing -> throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}

lookupUserById :: UUID -> MyHandler S.User
lookupUserById uid = do
  mUser <- runDBSensitive $ D.getUserById uid
  case mUser of
    Just user -> pure user
    Nothing -> throwError $ err404 {errBody = "User with ID " <> textToLBS (toText uid) <> " not found"}

-- | Build a UserMap from members of a specific group (including inactive members)
getGroupUserMap :: UUID -> MyHandler UserMap
getGroupUserMap gId = do
  users <- runDB $ D.getUsersForGroup gId
  pure $ M.fromList [(S._userId u, schemaUserToApiUser u) | u <- users]

-- | An @ prefix restricts matching to handles; otherwise display names also match.
matchesUserQuery :: Text -> User -> Bool
matchesUserQuery rawQuery (User _ handle name) =
  case T.stripPrefix "@" query of
    Just handleQuery -> matchesQuery handleQuery handle
    Nothing -> matchesQuery query handle || matchesQuery query name
  where
    query = T.strip rawQuery

ensureGroupExists :: UUID -> MyHandler ()
ensureGroupExists gId = do
  exists <- runDB $ D.checkGroupExists gId
  unless exists $ throwError $ err404 {errBody = "Group with ID " <> textToLBS (toText gId) <> " not found"}

-- | Require ownership, independently of resource-existence checks at the call site.
ensureGroupOwner :: UUID -> UUID -> MyHandler ()
ensureGroupOwner userId groupId = do
  isOwner <- runDB $ D.isGroupOwner userId groupId
  unless isOwner $ throwError $ err403 {errBody = "Only the group owner can perform this action"}

-- | Require an active membership; historical membership alone does not grant access.
ensureGroupMember :: UUID -> UUID -> MyHandler ()
ensureGroupMember userId groupId = do
  isMember <- runDB $ D.isUserInGroup userId groupId
  unless isMember $ throwError $ err403 {errBody = "You are not a member of this group"}

-- | Enforce the stored name length contract without trimming or normalizing it.
validateGroupName :: Text -> MyHandler ()
validateGroupName gname = do
  when (T.null gname) $ throwError $ err400 {errBody = "Group name cannot be empty"}
  when (T.length gname > 100) $ throwError $ err400 {errBody = "Group name is too long"}

-- | Validate the selected UUID independently of mutable user labels.
ensureActiveGroupUser :: UUID -> UserId -> MyHandler ()
ensureActiveGroupUser gId (UserId uid) = do
  _ <- lookupUserById uid
  isMember <- runDB $ D.isUserInGroup uid gId
  unless isMember $
    throwError $
      err400 {errBody = "User with ID " <> textToLBS (toText uid) <> " is not a member of the group"}

-- | Scope record lookup to both IDs so another group's record is treated as absent.
ensureRecordExists :: UUID -> UUID -> MyHandler ()
ensureRecordExists gId rId = do
  exists <- runDB $ D.checkRecordExists gId rId
  unless exists $ throwError $ err404 {errBody = "Record with ID " <> textToLBS (toText rId) <> " not found"}

-- | Scope receipt lookup to the parent group, not just the receipt UUID.
ensureReceiptExists :: UUID -> UUID -> MyHandler ()
ensureReceiptExists gId rId = do
  exists <- runDB $ D.getReceipt gId rId
  case exists of
    Just _ -> pure ()
    Nothing -> throwError $ err404 {errBody = "Receipt with ID " <> textToLBS (toText rId) <> " not found"}
