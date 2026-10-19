{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Operator account provisioning and maintenance. Protected routes may target any
-- user, but enabled accounts cannot lose their last viable sign-in method.
module Jizhang.API.Admin.Users
  ( getAllUsers,
    createUser,
    bulkDeleteUsers,
    deleteUser,
    getAllAdmins,
    createAdmin,
    AdminUserManagementAPI,
    adminUserManagementServer,
  )
where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Either (isRight)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID (toText)
import qualified Data.UUID.V4 as UUID
import Jizhang.API.Admin.Common
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceExpensiveLimit)
import Jizhang.RequestProtection (Principal (AdminPrincipal))
import Jizhang.API.Types
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.API.Internal.Error (textToLBS)
import Jizhang.API.User.Account (normalizeUsername, normalizeDisplayName, normalizeEmail, validatePasswordText, hashPasswordText)
import Jizhang.API.User.Projection (schemaUserToProfile)
import Jizhang.API.Internal.Resource (lookupUserById, matchesUserQuery)
import Jizhang.Auth.Context (identitySummary, providerSettingsFromRow)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.AdminAuthentication as AD
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.OIDC as O
import Log.Class
import Servant

-- These full paths are appended to the protected operator API, never the user API.
type AdminUserManagementAPI =
  "admin" :> "users" :> Capture "userId" UserId :> Get '[JSON] DTO.AdminUserDetails
    :<|> "admin" :> "users" :> Capture "userId" UserId :> ReqBody '[JSON] DTO.AdminUpdateUserRequest :> Put '[JSON] DTO.AdminUserDetails
    :<|> "admin" :> "users" :> Capture "userId" UserId :> "password" :> Verb 'DELETE 204 '[JSON] NoContent
    :<|> "admin" :> "users" :> Capture "userId" UserId :> "identity" :> Verb 'DELETE 204 '[JSON] NoContent
    :<|> "admin" :> "users" :> Capture "userId" UserId :> "revoke-sessions" :> Verb 'POST 204 '[JSON] NoContent

adminUserManagementServer :: AuthAdmin -> ServerT AdminUserManagementAPI MyHandler
adminUserManagementServer admin =
  getUserDetails admin
    :<|> updateUser admin
    :<|> removePassword admin
    :<|> removeIdentity admin
    :<|> revokeSessions admin

getAllUsers :: AuthAdmin -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> MyHandler (PaginatedResponse DTO.AdminUserSummary)
getAllUsers authAdmin mQuery mOffset mLimit mSort =
  paginatedListHandler
    ("Listing all users for admin: " <> authAdminUsername authAdmin)
    (runDBSensitive AD.getAdminUserRows)
    ( \(user, identity) ->
        DTO.AdminUserSummary
          (schemaUserToProfile user)
          (S._enabled user)
          (isJust (S._passwordHash user))
          (isJust identity)
    )
    filterUsers
    sortUsers
    mQuery
    mOffset
    mLimit
    mSort

-- | Local provisioning requires a password; SSO provisioning requires configured
-- provider settings and rechecks that configuration inside the insert transaction.
createUser :: AuthAdmin -> AdminCreateUserRequest -> MyHandler DTO.AdminUserDetails
createUser authAdmin AdminCreateUserRequest {..} = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  username <- normalizeUsername createUsername
  displayName <- normalizeDisplayName createDisplayName
  email <- normalizeEmail createEmail
  passwordHash <- case (createMode, createPassword) of
    (AdminCreateLocal, Just password) -> validatePasswordText password >> Just <$> hashPasswordText password
    (AdminCreateLocal, Nothing) -> throwError $ err400 {errBody = "Local provisioning requires a password"}
    (AdminCreateSso, Nothing) -> pure Nothing
    (AdminCreateSso, Just _) -> throwError $ err400 {errBody = "SSO provisioning requires a null password"}
  runtime <- asks appAuthRuntime
  settings <- runDBSensitive A.getAuthSettings
  unless (createMode /= AdminCreateSso || isRight (providerSettingsFromRow runtime settings)) $
    throwError $
      err409 {errBody = "Enable and configure SSO before provisioning an SSO user"}
  uid <- liftIO UUID.nextRandom
  let user = S.User uid username displayName email False passwordHash True Nothing
  result <- runDBSensitiveTransaction $ do
    currentSettings <- A.getAuthSettings
    -- Do not create a passwordless account against provider settings replaced during preparation.
    unless (createMode /= AdminCreateSso || currentSettings == settings) A.abortAuthTransaction
    conflict <- AD.userProfileConflict uid username email
    case conflict of
      Just message -> pure $ Left (err409 {errBody = textToLBS message})
      Nothing -> do
        AD.insertAdminUser user
        pure $ Right (details currentSettings user Nothing [])
  either throwError pure result

getUserDetails :: AuthAdmin -> UserId -> MyHandler DTO.AdminUserDetails
getUserDetails _ (UserId uid) = do
  now <- clock
  result <- runDBSensitiveTransaction $ do
    user <- D.getUserById uid
    case user of
      Nothing -> pure $ Left err404
      Just row -> do
        settings <- A.getAuthSettings
        identity <- A.getUserIdentity uid
        invitations <- AD.pendingUserInvitations uid now
        pure $ Right (details settings row identity invitations)
  either throwError pure result

-- | Changing email clears verification; disabling an account revokes authentication
-- in the same transaction as its normalized profile update.
updateUser :: AuthAdmin -> UserId -> DTO.AdminUpdateUserRequest -> MyHandler DTO.AdminUserDetails
updateUser _ (UserId uid) (DTO.AdminUpdateUserRequest rawUsername rawDisplayName rawEmail enabled) = do
  username <- normalizeUsername rawUsername
  displayName <- normalizeDisplayName rawDisplayName
  email <- normalizeEmail rawEmail
  now <- clock
  result <- runDBSensitiveTransaction $ do
    current <- D.getUserById uid
    case current of
      Nothing -> pure $ Left err404
      Just user -> do
        conflict <- AD.userProfileConflict uid username email
        case conflict of
          Just message -> pure $ Left (err409 {errBody = textToLBS message})
          Nothing -> do
            settings <- A.getAuthSettings
            identity <- A.getUserIdentity uid
            invitations <- AD.pendingUserInvitations uid now
            let disabling = S._enabled user && not enabled
                replacement =
                  user
                    { S._username = username,
                      S._displayName = displayName,
                      S._email = email,
                      S._emailVerified = S._emailVerified user && email == S._email user,
                      S._enabled = enabled
                    }
            A.saveUserAuthentication user replacement
            -- Disabling and revocation commit together so existing sessions cannot outlive the disable.
            if disabling then A.revokeUserAuthentication uid now else pure ()
            pure $ Right (details settings replacement identity (if disabling then [] else invitations))
  either throwError pure result

-- | Enabled accounts must retain a linked identity for the configured provider.
removePassword :: AuthAdmin -> UserId -> MyHandler NoContent
removePassword _ (UserId uid) = do
  runtime <- asks appAuthRuntime
  settings <- runDBSensitive A.getAuthSettings
  let providerReady = isRight (providerSettingsFromRow runtime settings)
  now <- clock
  result <- runDBSensitiveTransaction $ do
    currentSettings <- A.getAuthSettings
    unless (settings == currentSettings) A.abortAuthTransaction
    current <- D.getUserById uid
    identity <- A.getUserIdentity uid
    case current of
      Nothing -> pure $ Left err404
      Just user
        -- Disabled accounts may lose all methods; enabled ones need a currently usable provider identity.
        | S._enabled user
            && not
              ( providerReady
                  && maybe
                    False
                    (\row -> S._settingsIssuer settings == Just (S._identityIssuer row))
                    identity
              ) ->
            pure $ Left lastMethodError
        | otherwise -> do
            A.saveUserAuthentication user (user {S._passwordHash = Nothing})
            A.revokeUserAuthentication uid now
            pure $ Right NoContent
  either throwError pure result

-- | Enabled accounts must retain a password; removal also revokes authentication.
removeIdentity :: AuthAdmin -> UserId -> MyHandler NoContent
removeIdentity _ (UserId uid) = do
  now <- clock
  result <- runDBSensitiveTransaction $ do
    current <- D.getUserById uid
    identity <- A.getUserIdentity uid
    case current of
      Nothing -> pure $ Left err404
      Just user
        -- The remaining method must be local: this transaction is deleting the external fallback.
        | S._enabled user && not (isJust (S._passwordHash user)) -> pure $ Left lastMethodError
        | otherwise -> do
            mapM_ A.deleteExternalIdentity identity
            A.revokeUserAuthentication uid now
            pure $ Right NoContent
  either throwError pure result

revokeSessions :: AuthAdmin -> UserId -> MyHandler NoContent
revokeSessions _ (UserId uid) = do
  now <- clock
  result <- runDBSensitiveTransaction $ do
    current <- D.getUserById uid
    case current of
      Nothing -> pure $ Left err404
      Just user -> AD.revokeAdminUserSessions user now >> pure (Right NoContent)
  either throwError pure result

lastMethodError :: ServerError
lastMethodError = err409 {errBody = "Cannot remove the last sign-in method"}

clock :: MyHandler UTCTime
clock = asks appAuthRuntime >>= liftIO . O.authClock

-- | Expose method presence and pending invitation metadata, never credential hashes
-- or invitation bearer tokens.
details :: S.AuthSettings -> S.User -> Maybe S.ExternalIdentity -> [S.AccountInvitation] -> DTO.AdminUserDetails
details settings user identity invitations =
  DTO.AdminUserDetails
    (schemaUserToProfile user)
    (S._enabled user)
    (isJust (S._passwordHash user))
    (identitySummary (S._settingsProviderName settings) <$> identity)
    [ DTO.PendingInvitation
        (S._invitationId row)
        (if S._invitationPurpose row == "password_reset" then DTO.PasswordResetInvitation else DTO.OidcLinkInvitation)
        (S._invitationExpiresAt row)
    | row <- invitations
    ]

bulkDeleteUsers :: AuthAdmin -> BulkRequest UserId -> MyHandler NoContent
bulkDeleteUsers authAdmin (BulkRequest userIds) =
  bulkDeleteHandler
    ("Bulk deleting users as admin: " <> authAdminUsername authAdmin)
    userIds
    (\(UserId uid) -> void $ lookupUserById uid)
    (\(UserId uid) -> D.deleteUser uid)

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
createAdmin authAdmin AdminCreateAdminRequest {..} = do
  enforceExpensiveLimit (AdminPrincipal $ authAdminId authAdmin)
  createWithPasswordHash
    ("Creating admin as admin: " <> authAdminUsername authAdmin <> " target=" <> createAdminUsername)
    createAdminUsername
    createAdminPassword
    (isJust <$> runDB (D.getAdminByUsername createAdminUsername))
    "Admin already exists"
    (\username passwordHash -> runDBSensitiveTransaction $ D.insertAdmin username passwordHash)
    schemaAdminToSummary

schemaAdminToSummary :: S.Admin -> AdminSummary
schemaAdminToSummary admin = AdminSummary (S._adminId admin) (S._adminUsername admin)

filterUsers :: Maybe Text -> [DTO.AdminUserSummary] -> [DTO.AdminUserSummary]
filterUsers Nothing = id
filterUsers (Just queryText) = filter (\(DTO.AdminUserSummary (UserProfile publicUser _ _) _ _ _) -> matchesUserQuery queryText publicUser)

filterAdmins :: Maybe Text -> [AdminSummary] -> [AdminSummary]
filterAdmins Nothing = id
filterAdmins (Just queryText) = filter (matchesQuery queryText . summaryUsername)

userNameOf :: DTO.AdminUserSummary -> Text
userNameOf (DTO.AdminUserSummary (UserProfile (User _ uname _) _ _) _ _ _) = uname

sortUsers :: [DTO.AdminUserSummary] -> Maybe Text -> [DTO.AdminUserSummary]
sortUsers users Nothing = sortOn userNameOf users
sortUsers users (Just sortText) = applySort sortText userNameOf users

sortAdmins :: [AdminSummary] -> Maybe Text -> [AdminSummary]
sortAdmins admins Nothing = sortOn summaryUsername admins
sortAdmins admins (Just sortText) = applySort sortText summaryUsername admins
