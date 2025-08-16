{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Group where

import Control.Monad (when)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Jizhang.API.Types
import Jizhang.API.Utils
import Jizhang.Common.MyUUID
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

type GroupAPI =
  -- Get all groups
  "groups" :> Get '[JSON] [Group]
    -- Create a new group
    :<|> "groups" :> ReqBody '[JSON] Text :> Post '[JSON] Group
    -- Get a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> Get '[JSON] Group
    -- Update a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> ReqBody '[JSON] Text :> Put '[JSON] Group
    -- Delete a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> DeleteNoContent
    -- Add a member to a group
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> ReqBody '[JSON] User :> Post '[JSON] Group
    -- Delete a member from a group
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> Capture "username" User :> DeleteNoContent

groupServer :: MyServer GroupAPI
groupServer =
  getAllGroups
    :<|> createGroup
    :<|> getGroup True
    :<|> updateGroup
    :<|> deleteGroup
    :<|> addGroupMember
    :<|> deleteGroupMember

getAllGroups :: MyHandler [Group]
getAllGroups = do
  logInfo_ "Fetching all groups with members"
  gps <- runDB D.getAllGroupWithMembers
  let mp = M.fromListWith (++) [((S._groupId g, S._groupName g), [S.unUserId $ S._gmUser m' | m' <- maybeToList m]) | (g, m) <- gps]
  pure [Group (coerce gid) gn (coerce <$> uss) | ((gid, gn), uss) <- M.toList mp]

createGroup :: Text -> MyHandler Group
createGroup gname = do
  logInfo_ $ "Creating group: " <> gname
  validateGroupName gname
  group <- runDB $ D.insertGroup gname
  pure $ Group (coerce $ S._groupId group) (S._groupName group) [] -- No members initially

getGroup :: Bool -> GroupId -> MyHandler Group
getGroup checkExistence (GroupId gId) = do
  logInfo_ $ "Fetching group with ID: " <> T.pack (show gId)
  when checkExistence $ ensureGroupExists gId
  (g, ms) <- fmap fromJust <$> runDB $ D.getGroupWithMembers gId
  pure $ Group (coerce $ S._groupId g) (S._groupName g) (coerce . S.unUserId . S._gmUser <$> ms)

updateGroup :: GroupId -> Text -> MyHandler Group
updateGroup (GroupId gId) newName = do
  logInfo_ $ "Updating group " <> uuidToText gId <> " to new name: " <> newName
  ensureGroupExists gId
  validateGroupName newName
  runDB $ D.updateGroup gId (Just newName)
  getGroup False (GroupId gId)

deleteGroup :: GroupId -> MyHandler NoContent
deleteGroup (GroupId gId) = do
  logInfo_ $ "Deleting group with ID: " <> uuidToText gId
  ensureGroupExists gId
  _ <- runDB $ D.deleteGroup gId
  pure NoContent

addGroupMember :: GroupId -> User -> MyHandler Group
addGroupMember (GroupId gId) (User u) = do
  logInfo_ $ "Adding user " <> u <> " to group with ID: " <> uuidToText gId
  ensureGroupExists gId
  ensureUserExists u
  _ <- runDB $ D.addGroupMember u gId
  getGroup False (GroupId gId)

deleteGroupMember :: GroupId -> User -> MyHandler NoContent
deleteGroupMember (GroupId gId) (User u) = do
  logInfo_ $ "Removing user " <> u <> " from group with ID: " <> uuidToText gId
  ensureGroupExists gId
  isMember <- runDB $ D.isUserInGroup u gId
  if not isMember
    then throwError $ err404 {errBody = "User is not a member of the group"}
    else do
      _ <- runDB $ D.deleteGroupMember u gId
      pure NoContent
