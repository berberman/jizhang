{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.User where

import Data.Coerce (coerce)
import Jizhang.API.Types
import Jizhang.API.Utils
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

type UserAPI =
  -- Get all users
  "users"
    :> Get '[JSON] [User]
    -- Create a new user
    :<|> "users"
      :> ReqBody '[JSON] User
      :> Post '[JSON] User
    -- Delete a user
    :<|> "users"
      :> Capture "username" User
      :> DeleteNoContent
    -- Get a specific user
    :<|> "users"
      :> Capture "username" User
      :> Get '[JSON] User
    -- Get groups for a specific user
    :<|> "users"
      :> Capture "username" User
      :> "groups"
      :> Get '[JSON] [Group]

userServer :: MyServer UserAPI
userServer =
  getUsers
    :<|> createUser
    :<|> deleteUser
    :<|> getUser
    :<|> getGroupsForUser

getUsers :: MyHandler [User]
getUsers = do
  logInfo_ "Fetching all users"
  users <- runDB D.getAllUsers
  pure $ coerce <$> users

createUser :: User -> MyHandler User
createUser (User u) = do
  logInfo_ $ "Creating user: " <> u
  validateUsername u
  exists <- runDB $ D.checkUserExists u
  _ <-
    if exists
      then throwError $ err409 {errBody = "User already exists"}
      else runDB $ D.insertUser u
  pure $ User u

deleteUser :: User -> MyHandler NoContent
deleteUser (User u) = do
  logInfo_ $ "Deleting user: " <> u
  ensureUserExists u
  runDB $ D.deleteUser u
  pure NoContent

getUser :: User -> MyHandler User
getUser (User u) = do
  logInfo_ $ "Fetching user: " <> u
  ensureUserExists u
  pure $ User u

getGroupsForUser :: User -> MyHandler [Group]
getGroupsForUser (User u) = do
  logInfo_ $ "Fetching groups for user: " <> u
  ensureUserExists u
  groups <- runDB $ D.getGroupsForUser u
  gms <-
    mapM
      ( \g ->
          let gid = S._groupId g
              gname = S._groupName g
           in (gid,gname,) <$> runDB (D.getAllMembersInGroup gid)
      )
      groups
  pure [Group (coerce gid) gname (coerce . S.unUserId . S._gmUser <$> uss) | (gid, gname, uss) <- gms]
