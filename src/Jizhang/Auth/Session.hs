{-# LANGUAGE OverloadedStrings #-}

-- | Prepare JWT-backed sessions whose live database rows remain revocable.
-- Session creation belongs to the caller's transaction, not token signing.
module Jizhang.Auth.Session
  ( SessionAuthentication (..),
    prepareUserSession,
    requireActiveUserSession,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (addUTCTime)
import Data.UUID (toText)
import qualified Data.UUID.V4 as UUID
import Jizhang.API.App
import Jizhang.API.Types (AuthUser (..), LoginResponse (..))
import Jizhang.API.User.Projection (schemaUserToApiUser)
import Jizhang.Auth.Security (encryptSecret)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import Jizhang.OIDC
import Servant
import Servant.Auth.Server (makeJWT)

-- | Record the authentication method and, for OIDC, the already-verified provider proof.
-- Deliberately no Show instance: OIDC proof includes a sensitive ID token.
data SessionAuthentication = PasswordSession | OidcSession VerifiedOidcIdentity

-- | Prepare a one-day JWT and row before the caller's transaction; expose the
-- response only after insertion. OIDC logout hints use session-bound encryption.
prepareUserSession :: S.User -> SessionAuthentication -> MyHandler (S.UserSession, LoginResponse)
prepareUserSession user authentication = do
  runtime <- asks appAuthRuntime
  now <- liftIO $ authClock runtime
  sessionId <- liftIO UUID.nextRandom
  let expiry = addUTCTime (24 * 60 * 60) now
      claims = AuthUser (S._userId user) sessionId
  -- Signing is preparatory: callers must commit this exact session ID before returning the JWT.
  jwtSettings <- asks appJWTSettings
  signed <- liftIO $ makeJWT claims jwtSettings (Just expiry)
  token <- case signed of
    Left _ -> throwError $ err500 {errBody = "Failed to generate access token"}
    Right bytes -> pure $ TL.toStrict $ TLE.decodeUtf8 bytes
  (method, issuer, subject, sid, hint, reauthenticatedAt) <- case authentication of
    PasswordSession -> pure ("password", Nothing, Nothing, Nothing, Nothing, Nothing)
    OidcSession identity -> do
      key <- maybe (throwError $ err503 {errBody = "Configure OIDC_SECRET_KEY to enable OIDC"}) pure (authSecretKey runtime)
      -- Bind the logout hint to this row so copied ciphertext cannot impersonate another session.
      encrypted <- liftIO $ encryptSecret key (encodeUtf8 $ "user_sessions:" <> toText sessionId <> ":id_token") (verifiedIdToken identity)
      pure ("oidc", Just $ verifiedIssuer identity, Just $ verifiedSubject identity, verifiedSid identity, Just encrypted, verifiedReauthenticatedAt identity)
  let session =
        S.UserSession
          { S._sessionId = sessionId,
            S._sessionUser = S.UserId (S._userId user),
            S._sessionMethod = method,
            S._sessionCreatedAt = now,
            S._sessionAuthenticatedAt = now,
            S._sessionExpiresAt = expiry,
            S._sessionOidcReauthenticatedAt = reauthenticatedAt,
            S._sessionRevokedAt = Nothing,
            S._sessionOidcIssuer = issuer,
            S._sessionOidcSubject = subject,
            S._sessionOidcSid = sid,
            S._sessionIdTokenEncrypted = hint
          }
  pure (session, LoginResponse token (schemaUserToApiUser user))

-- | Check revocation, expiry and enablement at action execution, not server assembly.
-- Sensitive actions also use Context's account-invalidation and snapshot checks.
requireActiveUserSession :: AuthUser -> MyHandler ()
requireActiveUserSession claims = do
  runtime <- asks appAuthRuntime
  now <- liftIO $ authClock runtime
  -- A valid JWT alone is insufficient: the live row makes logout and account disable effective.
  active <- runDBSensitive $ do
    session <- A.getUserSession (authSessionId claims)
    user <- D.getUserById (authUserId claims)
    pure $ case (session, user) of
      (Just row, Just account) ->
        S._sessionUser row == S.UserId (authUserId claims)
          && S._sessionRevokedAt row == Nothing
          && S._sessionExpiresAt row > now
          && S._enabled account
      _ -> False
  unless active $ throwError err401
