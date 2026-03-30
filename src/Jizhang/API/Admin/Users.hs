{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Jizhang.API.Admin.Users
  ( getAllUsers,
    createUser,
    bulkDeleteUsers,
    deleteUser,
    getAllAdmins,
    createAdmin,
  )
where

import Control.Monad (void)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID (toText)
import Jizhang.API.Admin.Common
import Jizhang.API.Types
import Jizhang.API.Utils
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant (NoContent (..))

getAllUsers :: AuthAdmin -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse User)
getAllUsers authAdmin mQuery mOffset mLimit mSort =
  paginatedListHandler
    ("Listing all users for admin: " <> authAdminUsername authAdmin)
    (runDB D.getAllUsers)
    schemaUserToApiUser
    filterUsers
    sortUsers
    mQuery
    mOffset
    mLimit
    mSort

createUser :: AuthAdmin -> AdminCreateUserRequest -> MyHandler User
createUser authAdmin AdminCreateUserRequest {..} =
  createWithPasswordHash
    ("Creating user as admin: " <> authAdminUsername authAdmin <> " target=" <> createUsername)
    createUsername
    createPassword
    (runDB $ D.checkUserExists createUsername)
    "User already exists"
    (\username passwordHash -> runDB $ D.insertUser username passwordHash)
    schemaUserToApiUser

bulkDeleteUsers :: AuthAdmin -> BulkRequest UserId -> MyHandler NoContent
bulkDeleteUsers authAdmin (BulkRequest userIds) =
  bulkDeleteHandler
    ("Bulk deleting users as admin: " <> authAdminUsername authAdmin)
    userIds
    (\(UserId uid) -> void $ lookupUserById uid)
    (\(UserId uid) -> runDB $ D.deleteUser uid)

deleteUser :: AuthAdmin -> UserId -> MyHandler NoContent
deleteUser authAdmin (UserId uid) = do
  logInfo_ $ "Deleting user " <> toText uid <> " as admin: " <> authAdminUsername authAdmin
  _ <- lookupUserById uid
  runDB $ D.deleteUser uid
  pure NoContent

getAllAdmins :: AuthAdmin -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse AdminSummary)
getAllAdmins authAdmin mQuery mOffset mLimit mSort =
  paginatedListHandler
    ("Listing all admins for admin: " <> authAdminUsername authAdmin)
    (runDB D.getAllAdmins)
    schemaAdminToSummary
    filterAdmins
    sortAdmins
    mQuery
    mOffset
    mLimit
    mSort

createAdmin :: AuthAdmin -> AdminCreateAdminRequest -> MyHandler AdminSummary
createAdmin authAdmin AdminCreateAdminRequest {..} =
  createWithPasswordHash
    ("Creating admin as admin: " <> authAdminUsername authAdmin <> " target=" <> createAdminUsername)
    createAdminUsername
    createAdminPassword
    (isJust <$> runDB (D.getAdminByUsername createAdminUsername))
    "Admin already exists"
    (\username passwordHash -> runDB $ D.insertAdmin username passwordHash)
    schemaAdminToSummary

schemaAdminToSummary :: S.Admin -> AdminSummary
schemaAdminToSummary admin = AdminSummary (S._adminId admin) (S._adminUsername admin)

filterUsers :: Maybe Text -> [User] -> [User]
filterUsers Nothing = id
filterUsers (Just queryText) = filter (matchesQuery queryText . userNameOf)

filterAdmins :: Maybe Text -> [AdminSummary] -> [AdminSummary]
filterAdmins Nothing = id
filterAdmins (Just queryText) = filter (matchesQuery queryText . summaryUsername)

userNameOf :: User -> Text
userNameOf (User _ uname) = uname

sortUsers :: [User] -> Maybe Text -> [User]
sortUsers users Nothing = sortOn userNameOf users
sortUsers users (Just sortText) = applySort sortText userNameOf users

sortAdmins :: [AdminSummary] -> Maybe Text -> [AdminSummary]
sortAdmins admins Nothing = sortOn summaryUsername admins
sortAdmins admins (Just sortText) = applySort sortText summaryUsername admins
