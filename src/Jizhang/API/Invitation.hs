{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Public inspection and redemption of one-use account invitations. Tokens are
-- bearer grants; responses use one invalid-or-expired error for rejected grants.
module Jizhang.API.Invitation
  ( InvitationAPI,
    invitationServer,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Jizhang.API.App
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.API.User.Account (validatePasswordText, hashPasswordText)
import Jizhang.API.User.Projection (schemaUserToApiUser)
import Jizhang.Auth.Security (secureEqual, tokenDigest)
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Database.User as U
import qualified Jizhang.OIDC as O
import Servant

type InvitationAPI =
  "auth"
    :> ( "invitations" :> "inspect" :> ReqBody '[JSON] DTO.InvitationInspectRequest :> Post '[JSON] DTO.InvitationInspection
           :<|> "password-reset" :> ReqBody '[JSON] DTO.PasswordResetRequest :> PostNoContent
       )

invitationServer :: MyServer InvitationAPI
invitationServer = inspectInvitation :<|> resetPassword

-- | Inspect without consuming; SSO-link grants must match the current provider revision.
inspectInvitation :: DTO.InvitationInspectRequest -> MyHandler DTO.InvitationInspection
inspectInvitation (DTO.InvitationInspectRequest token) = do
  validateInvitationToken token
  now <- currentTime
  result <- runDBSensitive $ do
    invitation <- A.getAccountInvitationByTokenHash (tokenDigest token)
    case invitation of
      Just row | validInvitation now token row -> do
        user <- U.getUserById (S.unUserId $ S._invitationUser row)
        settings <- A.getAuthSettings
        pure $ case user of
          Just target
            | S._invitationPurpose row == "password_reset" ->
                Just $ DTO.InvitationInspection DTO.PasswordResetInvitation (schemaUserToApiUser target) (S._invitationExpiresAt row)
          Just target
            | S._invitationPurpose row == "oidc_link",
              S._enabled target,
              S._settingsOidcEnabled settings,
              S._invitationProviderRevision row == Just (S._settingsRevision settings) ->
                Just $ DTO.InvitationInspection DTO.OidcLinkInvitation (schemaUserToApiUser target) (S._invitationExpiresAt row)
          _ -> Nothing
      _ -> pure Nothing
  maybe invalidInvitation pure result

-- | Hash outside the transaction, then consume the unchanged grant and revoke old
-- authentication atomically. A reset does not enable a disabled account.
resetPassword :: DTO.PasswordResetRequest -> MyHandler NoContent
resetPassword (DTO.PasswordResetRequest token password) = do
  validateInvitationToken token
  now <- currentTime
  snapshot <- runDBSensitive $ do
    invitation <- A.getAccountInvitationByTokenHash (tokenDigest token)
    case invitation of
      Just row
        | validInvitation now token row,
          S._invitationPurpose row == "password_reset" -> do
            user <- U.getUserById (S.unUserId $ S._invitationUser row)
            case user of
              Just target -> do
                identity <- A.getUserIdentity (S._userId target)
                settings <- A.getAuthSettings
                pure $ Just (row, target, identity, settings)
              Nothing -> pure Nothing
      _ -> pure Nothing
  (invitation, user, identity, settings) <- maybe invalidInvitation pure snapshot
  validatePasswordText password
  -- Bcrypt can outlive the grant or a concurrent credential change; the reads above
  -- are only a snapshot, so validity is checked again with the post-hash clock.
  hash <- hashPasswordText password
  committedAt <- currentTime
  success <- runDBSensitiveTransaction $ do
    currentInvitation <- A.getAccountInvitation (S._invitationId invitation)
    case currentInvitation of
      Just grant
        | grant == invitation,
          validInvitation committedAt token grant -> do
            currentUser <- U.getUserById (S._userId user)
            currentIdentity <- A.getUserIdentity (S._userId user)
            currentSettings <- A.getAuthSettings
            -- A reset may target a disabled account, but cannot overwrite a changed
            -- credential snapshot or restore an account's enabled state.
            unless
              (currentUser == Just user && currentIdentity == identity && currentSettings == settings)
              A.abortAuthTransaction
            -- Consume before account-wide revocation; otherwise revocation would alter
            -- this same grant and make its snapshot update fail inside the transaction.
            A.consumeAccountInvitation grant committedAt
            A.saveUserAuthentication user (user {S._passwordHash = Just hash})
            A.revokeUserAuthentication (S._userId user) committedAt
            A.cleanupExpiredAuth committedAt
            pure True
      _ -> pure False
  unless success invalidInvitation
  pure NoContent

-- | Require matching digest, known purpose, future expiry and no prior consumption/revocation.
validInvitation :: UTCTime -> Text -> S.AccountInvitation -> Bool
validInvitation now token row =
  secureEqual (tokenDigest token) (S._invitationTokenHash row)
    && S._invitationExpiresAt row > now
    && S._invitationConsumedAt row == Nothing
    && S._invitationRevokedAt row == Nothing
    && S._invitationPurpose row `elem` ["password_reset", "oidc_link"]

-- Bearer grants are exactly the canonical, unpadded encoding of 32 random bytes.
validateInvitationToken :: Text -> MyHandler ()
validateInvitationToken token = unless canonical invalidInvitation
  where
    canonical = case B64URL.decodeUnpadded (encodeUtf8 token) of
      Right bytes -> BS.length bytes == 32 && B64URL.encodeUnpadded bytes == encodeUtf8 token
      Left _ -> False

invalidInvitation :: MyHandler a
invalidInvitation = throwError $ err400 {errBody = "This invitation is invalid or expired"}

currentTime :: MyHandler UTCTime
currentTime = asks appAuthRuntime >>= liftIO . O.authClock
