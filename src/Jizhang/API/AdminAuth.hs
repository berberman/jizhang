{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Operator password authentication and browser-cookie sessions, separate from
-- user sessions. Token and cookie login both issue credentials valid for 24 hours.
module Jizhang.API.AdminAuth
  ( AdminAuthAPI,
    adminAuthServer,
    adminCookieSettings,
    bootstrapAdmin,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (Serializable), ReadWriteMode (ReadWrite), TransactionMode (..), withTransactionMode)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceAdminCredentialLimit, enforcePrincipalLimit)
import Jizhang.API.Types
import Jizhang.Database.Admin (ensureAdminExists, getAdminByUsername)
import Jizhang.Database.Connection (withConnection)
import qualified Jizhang.Database.Schema as S
import Jizhang.RequestProtection (Principal (AdminPrincipal))
import Log (LogT)
import Log.Class
import Servant
import Servant.Auth (Auth, Cookie, JWT)
import Servant.Auth.Server (AuthResult (Authenticated), CookieSettings (..), acceptLogin, clearSession, defaultCookieSettings, makeJWT)
import Web.Cookie (SetCookie)

-- | Support bearer login and browser session login; session inspection requires
-- authentication, while unauthenticated logout returns without clearing headers.
type AdminAuthAPI =
  "admin"
    :> "auth"
    :> "login"
    :> ReqBody '[JSON] AdminLoginRequest
    :> Post '[JSON] AdminLoginResponse
    :<|> "admin"
      :> "auth"
      :> "session"
      :> "login"
      :> ReqBody '[JSON] AdminLoginRequest
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AdminSummary)
    :<|> Auth '[JWT, Cookie] AuthAdmin
      :> "admin"
      :> "auth"
      :> "session"
      :> "logout"
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> Auth '[JWT, Cookie] AuthAdmin :> "admin" :> "auth" :> "session" :> "me" :> Get '[JSON] AdminSummary

adminAuthServer :: MyServer AdminAuthAPI
adminAuthServer = adminLogin :<|> adminSessionLogin :<|> adminSessionLogout :<|> adminSessionMe

-- | Only the deployment's secure-cookie flag differs from Servant's defaults.
adminCookieSettings :: Bool -> CookieSettings
adminCookieSettings secure = defaultCookieSettings {cookieIsSecure = if secure then Secure else NotSecure}

adminLogin :: AdminLoginRequest -> MyHandler AdminLoginResponse
adminLogin loginRequest = do
  admin <- authenticateAdmin loginRequest
  issueAdminToken admin

adminSessionLogin :: AdminLoginRequest -> MyHandler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AdminSummary)
adminSessionLogin loginRequest = do
  admin <- authenticateAdmin loginRequest
  jwtSettings <- asks appJWTSettings
  cookieSettings <- adminCookieSettings <$> asks appAdminCookieSecure
  now <- liftIO getCurrentTime
  let claims = AuthAdmin (S._adminId admin) (S._adminUsername admin)
      -- Cookie persistence is explicit here; bearer login sets the JWT expiry separately below.
      sessionSettings = cookieSettings {cookieExpires = Just $ addUTCTime (24 * 60 * 60) now}
  mApplyCookies <- liftIO $ acceptLogin sessionSettings jwtSettings claims
  case mApplyCookies of
    Nothing -> throwError $ err500 {errBody = "Failed to create admin session"}
    Just applyCookies -> pure $ applyCookies $ AdminSummary (S._adminId admin) (S._adminUsername admin)

adminSessionLogout :: AuthResult AuthAdmin -> MyHandler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
adminSessionLogout (Authenticated _) = do
  cookieSettings <- adminCookieSettings <$> asks appAdminCookieSecure
  -- Clearing browser cookies does not revoke a bearer token already issued to this operator.
  pure $ clearSession cookieSettings NoContent
adminSessionLogout _ = pure $ noHeader $ noHeader NoContent

adminSessionMe :: AuthResult AuthAdmin -> MyHandler AdminSummary
adminSessionMe (Authenticated authAdmin) = do
  enforcePrincipalLimit (AdminPrincipal (authAdminId authAdmin))
  pure $ AdminSummary (authAdminId authAdmin) (authAdminUsername authAdmin)
adminSessionMe _ = throwError err401

-- | Rate-limit known operators by UUID and unknown names by digest before bcrypt.
authenticateAdmin :: AdminLoginRequest -> MyHandler S.Admin
authenticateAdmin AdminLoginRequest {..} = do
  logInfo_ "Operator password login attempt"
  mAdmin <- runDBSensitive $ getAdminByUsername username
  enforceAdminCredentialLimit (S._adminId <$> mAdmin) username
  case mAdmin of
    Nothing -> throwError $ err401 {errBody = "Invalid admin username or password"}
    Just admin -> do
      let valid = validatePassword (encodeUtf8 $ S._adminPasswordHash admin) (encodeUtf8 password)
      if valid then pure admin else throwError $ err401 {errBody = "Invalid admin username or password"}

issueAdminToken :: S.Admin -> MyHandler AdminLoginResponse
issueAdminToken admin = do
  jwtSettings <- asks appJWTSettings
  now <- liftIO getCurrentTime
  let expiry = addUTCTime (24 * 60 * 60) now
      claims = AuthAdmin (S._adminId admin) (S._adminUsername admin)
  eToken <- liftIO $ makeJWT claims jwtSettings (Just expiry)
  case eToken of
    Left _ -> throwError $ err500 {errBody = "Failed to generate admin access token"}
    Right tokenBS ->
      pure AdminLoginResponse {accessToken = TL.toStrict $ TLE.decodeUtf8 tokenBS, adminUsername = S._adminUsername admin}

-- | Provision a configured bootstrap operator under a serializable transaction;
-- absence of bootstrap configuration leaves persisted operators untouched.
bootstrapAdmin :: AppEnv -> LogT IO ()
bootstrapAdmin appEnv = case appAdminBootstrap appEnv of
  Nothing -> pure ()
  Just AdminBootstrap {..} -> do
    let rawPassword = encodeUtf8 adminBootstrapPassword
    when (BS.length rawPassword > 72) $ fail "Password must be at most 72 UTF-8 bytes"
    -- Pay bcrypt's cost before acquiring the serializable bootstrap transaction.
    mHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy rawPassword
    case mHash of
      Nothing -> fail "Failed to hash bootstrap admin password"
      Just hash -> do
        liftIO $ withConnection (appConn appEnv) $ \conn ->
          withTransactionMode (TransactionMode Serializable ReadWrite) conn $
            ensureAdminExists conn adminBootstrapUsername (decodeUtf8 hash)
