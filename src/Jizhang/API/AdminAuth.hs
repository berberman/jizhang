{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.AdminAuth
  ( AdminAuthAPI,
    adminAuthServer,
    adminCookieSettings,
    bootstrapAdmin,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (addUTCTime, getCurrentTime)
import Jizhang.API.Types
import Jizhang.Database.Admin (ensureAdminExists, getAdminByUsername)
import qualified Jizhang.Database.Schema as S
import Log (LogT)
import Log.Class
import Servant
import Servant.Auth (Auth, Cookie, JWT)
import Servant.Auth.Server (AuthResult (Authenticated), CookieSettings (..), acceptLogin, clearSession, defaultCookieSettings, makeJWT)
import Web.Cookie (SetCookie)

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
    :<|> "admin"
      :> "auth"
      :> "session"
      :> "logout"
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> Auth '[JWT, Cookie] AuthAdmin :> "admin" :> "auth" :> "session" :> "me" :> Get '[JSON] AdminSummary

adminAuthServer :: MyServer AdminAuthAPI
adminAuthServer = adminLogin :<|> adminSessionLogin :<|> adminSessionLogout :<|> adminSessionMe

adminCookieSettings :: CookieSettings
adminCookieSettings = defaultCookieSettings {cookieIsSecure = NotSecure}

adminLogin :: AdminLoginRequest -> MyHandler AdminLoginResponse
adminLogin loginRequest = do
  admin <- authenticateAdmin loginRequest
  issueAdminToken admin

adminSessionLogin :: AdminLoginRequest -> MyHandler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AdminSummary)
adminSessionLogin loginRequest = do
  admin <- authenticateAdmin loginRequest
  jwtSettings <- asks appJWTSettings
  let claims = AuthAdmin (S._adminId admin) (S._adminUsername admin)
  mApplyCookies <- liftIO $ acceptLogin adminCookieSettings jwtSettings claims
  case mApplyCookies of
    Nothing -> throwError $ err500 {errBody = "Failed to create admin session"}
    Just applyCookies -> pure $ applyCookies $ AdminSummary (S._adminId admin) (S._adminUsername admin)

adminSessionLogout :: MyHandler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
adminSessionLogout = pure $ clearSession adminCookieSettings NoContent

adminSessionMe :: AuthResult AuthAdmin -> MyHandler AdminSummary
adminSessionMe (Authenticated authAdmin) = pure $ AdminSummary (authAdminId authAdmin) (authAdminUsername authAdmin)
adminSessionMe _ = throwError err401

authenticateAdmin :: AdminLoginRequest -> MyHandler S.Admin
authenticateAdmin AdminLoginRequest {..} = do
  logInfo_ $ "Admin login attempt for: " <> username
  mAdmin <- runDB $ getAdminByUsername username
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

bootstrapAdmin :: AppEnv -> LogT IO ()
bootstrapAdmin appEnv = case appAdminBootstrap appEnv of
  Nothing -> pure ()
  Just AdminBootstrap {..} -> do
    let rawPassword = encodeUtf8 adminBootstrapPassword
    mHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy rawPassword
    case mHash of
      Nothing -> fail "Failed to hash bootstrap admin password"
      Just hash -> do
        let conn = appConn appEnv
        liftIO $ ensureAdminExists conn adminBootstrapUsername (decodeUtf8 hash)
