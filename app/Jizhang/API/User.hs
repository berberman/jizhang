{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.User where

import Control.Monad (forM)
import Data.Coerce (coerce)
import Jizhang.API.Types
import Jizhang.API.Utils
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

type UserAPI =
  -- Get the authenticated user
  "users"
    :> "me"
    :> Get '[JSON] User
    -- Delete the authenticated user
    :<|> "users"
      :> "me"
      :> Delete '[JSON] NoContent
    -- Get groups for the authenticated user
    :<|> "users"
      :> "me"
      :> "groups"
      :> Get '[JSON] [Group]

userServer :: AuthUser -> MyServer UserAPI
userServer authUser =
  getMe authUser
    :<|> deleteMe authUser
    :<|> getMyGroups authUser

getMe :: AuthUser -> MyHandler User
getMe AuthUser {..} = do
  logInfo_ $ "Fetching current user: " <> authUsername
  sUser <- lookupUserById authUserId
  pure $ schemaUserToApiUser sUser

deleteMe :: AuthUser -> MyHandler NoContent
deleteMe AuthUser {..} = do
  logInfo_ $ "Deleting current user: " <> authUsername
  runDB $ D.deleteUser authUserId
  pure NoContent

getMyGroups :: AuthUser -> MyHandler [Group]
getMyGroups AuthUser {..} = do
  logInfo_ $ "Fetching groups for current user: " <> authUsername
  groups <- runDB $ D.getGroupsForUser authUserId
  forM groups $ \g -> do
    let gid = S._groupId g
    um <- getGroupUserMap gid
    ms <- runDB $ D.getAllMembersInGroup gid
    pure $ Group (coerce gid) (S._groupName g) (resolveUser um (S.unUserId $ S._groupOwner g)) (resolveGroupMembers um ms)

-- | Resolve group members to API Users using the user map
resolveGroupMembers :: UserMap -> [S.GroupMember] -> [User]
resolveGroupMembers um = map (\m -> resolveUser um (S.unUserId $ S._gmUser m))
