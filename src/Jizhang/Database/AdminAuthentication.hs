{-# LANGUAGE OverloadedStrings #-}

-- | Operator authentication queries and revocation writes. Callers must use the
-- sensitive runner and Serializable transactions; these Pg actions enforce neither.
module Jizhang.Database.AdminAuthentication where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Postgres (Pg)
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Database.User as U

getAllIdentities :: Pg [S.ExternalIdentity]
getAllIdentities = runSelectReturningList $ select $ all_ (S._externalIdentities S.jizhangDb)

-- | Flatten users and linked identities, retaining users without a link.
-- Multiple identities repeat the user row; this is not a unique-user listing.
getAdminUserRows :: Pg [(S.User, Maybe S.ExternalIdentity)]
getAdminUserRows = runSelectReturningList $ select $ do
  user <- all_ (S._users S.jizhangDb)
  identity <-
    leftJoin_
      (all_ (S._externalIdentities S.jizhangDb))
      (\row -> S._identityUser row ==. primaryKey user)
  pure (user, identity)

-- | Count enabled, passwordless accounts linked to the configured issuer.
-- Missing issuer yields zero; provider enablement is not part of this query.
getSsoOnlyUserCount :: S.AuthSettings -> Pg Int
getSsoOnlyUserCount settings = case S._settingsIssuer settings of
  Nothing -> pure 0
  Just issuer ->
    (fromIntegral . fromMaybe (0 :: Int64))
      <$> runSelectReturningOne
        ( select $ aggregate_ (const countAll_) $ do
            user <- all_ (S._users S.jizhangDb)
            identity <- all_ (S._externalIdentities S.jizhangDb)
            guard_
              ( S._enabled user
                  &&. isNothing_ (S._passwordHash user)
                  &&. S._identityUser identity
                  ==. primaryKey user
                  &&. S._identityIssuer identity
                  ==. val_ issuer
              )
            pure (S._userId user)
        )

-- | Exclude the immutable target UUID for both create and profile edits.
userProfileConflict :: UUID -> Text -> Text -> Pg (Maybe Text)
userProfileConflict target username email = do
  usernameOwner <- U.getUserByUsername username
  emailOwner <- U.getUserByEmail email
  pure $
    if maybe False ((/= target) . S._userId) usernameOwner
      then Just "Username is already in use"
      else
        if maybe False ((/= target) . S._userId) emailOwner
          then Just "Email is already in use"
          else Nothing

-- | Insert a caller-validated complete account; this does not issue an invitation
-- or create credentials, sessions or an external identity.
insertAdminUser :: S.User -> Pg ()
insertAdminUser = U.insertUserRow

pendingUserInvitations :: UUID -> UTCTime -> Pg [S.AccountInvitation]
pendingUserInvitations = A.listPendingUserInvitations

-- | Revoke-all preserves outstanding operator invitations. The fence also
-- rejects login results whose account was not yet known when revocation ran.
revokeAdminUserSessions :: S.User -> UTCTime -> Pg ()
revokeAdminUserSessions expected now = do
  A.saveUserAuthentication expected (expected {S._authInvalidatedAt = Just now})
  A.revokeUserSessions (S._userId expected) now
  A.failUserTransactions (S._userId expected)
