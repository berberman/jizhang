{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.AdminAuth
  ( AdminAuthAPI,
    adminAuthServer,
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
import Servant.Auth.Server (makeJWT)

type AdminAuthAPI =
  "admin"
    :> "auth"
    :> "login"
    :> ReqBody '[JSON] AdminLoginRequest
    :> Post '[JSON] AdminLoginResponse

adminAuthServer :: MyServer AdminAuthAPI
adminAuthServer = adminLogin

adminLogin :: AdminLoginRequest -> MyHandler AdminLoginResponse
adminLogin AdminLoginRequest {..} = do
  logInfo_ $ "Admin login attempt for: " <> username
  mAdmin <- runDB $ getAdminByUsername username
  case mAdmin of
    Nothing -> throwError $ err401 {errBody = "Invalid admin username or password"}
    Just admin -> do
      let valid = validatePassword (encodeUtf8 $ S._adminPasswordHash admin) (encodeUtf8 password)
      if not valid
        then throwError $ err401 {errBody = "Invalid admin username or password"}
        else do
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
