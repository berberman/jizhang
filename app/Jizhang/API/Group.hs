{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Group
  ( GroupAPI,
    groupServer,
    getGroup,
  )
where

import Control.Monad (unless, void, when)
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Jizhang.API.Types
import Jizhang.API.Utils
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

type GroupAPI =
  -- Create a new group
  "groups" :> ReqBody '[JSON] Text :> Post '[JSON] Group
    -- Get a specific group (requires membership)
    :<|> "groups" :> Capture "groupId" GroupId :> Get '[JSON] Group
    -- Update a specific group (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> ReqBody '[JSON] Text :> Put '[JSON] Group
    -- Delete a specific group (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> Delete '[JSON] NoContent
    -- Add a member to a group (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> ReqBody '[JSON] Username :> Post '[JSON] Group
    -- Delete a member from a group (requires ownership or self-deletion)
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> Capture "username" Username :> Delete '[JSON] NoContent
    -- Transfer group ownership (requires ownership)
    :<|> "groups" :> Capture "groupId" GroupId :> "owner" :> ReqBody '[JSON] Username :> Put '[JSON] Group

groupServer :: AuthUser -> MyServer GroupAPI
groupServer authUser =
  createGroup authUser
    :<|> getGroupEndpoint authUser
    :<|> updateGroup authUser
    :<|> deleteGroup authUser
    :<|> addGroupMember authUser
    :<|> deleteGroupMember authUser
    :<|> transferOwnership authUser

-- | Get a group by ID, checking that the authenticated user is a member
getGroupEndpoint :: AuthUser -> GroupId -> MyHandler Group
getGroupEndpoint authUser (GroupId gId) = do
  ensureGroupExists gId
  ensureGroupMember (authUserId authUser) gId
  getGroup (GroupId gId)

createGroup :: AuthUser -> Text -> MyHandler Group
createGroup authUser gname = do
  logInfo_ $ "Creating group: " <> gname
  validateGroupName gname
  let ownerUid = authUserId authUser
  group <- runDB $ D.insertGroup gname ownerUid
  -- Auto-add owner as a member
  _ <- runDB $ D.addGroupMember ownerUid (S._groupId group)
  let ownerApiUser = User (coerce ownerUid) (authUsername authUser)
  pure $ Group (coerce $ S._groupId group) (S._groupName group) ownerApiUser [ownerApiUser]

getGroup :: GroupId -> MyHandler Group
getGroup (GroupId gId) = do
  logInfo_ $ "Fetching group with ID: " <> T.pack (show gId)
  (g, ms) <- fetchOrFail "Group" gId $ runDB $ D.getGroupWithMembers gId
  um <- getGroupUserMap gId
  pure $ Group (coerce $ S._groupId g) (S._groupName g) (resolveUser um (S.unUserId $ S._groupOwner g)) [resolveUser um (S.unUserId $ S._gmUser m) | m <- ms]

updateGroup :: AuthUser -> GroupId -> Text -> MyHandler Group
updateGroup authUser (GroupId gId) newName = do
  logInfo_ $ "Updating group " <> toText gId <> " to new name: " <> newName
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  validateGroupName newName
  runDB $ D.updateGroup gId (Just newName)
  getGroup (GroupId gId)

deleteGroup :: AuthUser -> GroupId -> MyHandler NoContent
deleteGroup authUser (GroupId gId) = do
  logInfo_ $ "Deleting group with ID: " <> toText gId
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  _ <- runDB $ D.deleteGroup gId
  pure NoContent

addGroupMember :: AuthUser -> GroupId -> Username -> MyHandler Group
addGroupMember authUser (GroupId gId) (Username u) = do
  logInfo_ $ "Adding user " <> u <> " to group with ID: " <> toText gId
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  sUser <- lookupUser u
  activeMember <- runDB $ D.isUserInGroup (S._userId sUser) gId
  when activeMember $ throwError $ err409 {errBody = "User is already a member of the group"}
  -- Re-activate if there is an existing inactive membership, otherwise insert new
  existingRow <- runDB $ D.isUserInGroupIncludingInactive (S._userId sUser) gId
  if existingRow
    then runDB $ D.reactivateGroupMember (S._userId sUser) gId
    else void $ runDB $ D.addGroupMember (S._userId sUser) gId
  getGroup (GroupId gId)

deleteGroupMember :: AuthUser -> GroupId -> Username -> MyHandler NoContent
deleteGroupMember authUser (GroupId gId) (Username u) = do
  logInfo_ $ "Deactivating user " <> u <> " from group with ID: " <> toText gId
  ensureGroupExists gId
  sUser <- lookupUser u
  let targetUid = S._userId sUser
  -- Allow the owner to deactivate any member, or a member to leave on their own
  isOwner <- runDB $ D.isGroupOwner (authUserId authUser) gId
  let isSelf = authUserId authUser == targetUid
  unless (isOwner || isSelf) $
    throwError $
      err403 {errBody = "Only the group owner or the member themselves can perform this action"}
  -- The owner cannot be deactivated
  targetIsOwner <- runDB $ D.isGroupOwner targetUid gId
  when targetIsOwner $
    throwError $
      err400 {errBody = "Cannot deactivate the group owner; transfer ownership first"}
  isMember <- runDB $ D.isUserInGroup targetUid gId
  unless isMember $
    throwError $
      err404 {errBody = "User is not an active member of the group"}
  runDB $ D.deactivateGroupMember targetUid gId
  pure NoContent

transferOwnership :: AuthUser -> GroupId -> Username -> MyHandler Group
transferOwnership authUser (GroupId gId) (Username u) = do
  logInfo_ $ "Transferring ownership of group " <> toText gId <> " to user " <> u
  ensureGroupExists gId
  ensureGroupOwner (authUserId authUser) gId
  sUser <- lookupUser u
  isMember <- runDB $ D.isUserInGroup (S._userId sUser) gId
  unless isMember $ throwError $ err400 {errBody = "New owner must be a member of the group"}
  runDB $ D.updateGroupOwner gId (S._userId sUser)
  getGroup (GroupId gId)
