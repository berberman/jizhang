{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API.Auth
  ( AuthAPI,
    authServer,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Jizhang.API.Types
import Jizhang.API.Utils
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant
import Servant.Auth.Server (makeJWT)

-- | JWT access token lifetime: 1 day
accessTokenLifetime :: NominalDiffTime
accessTokenLifetime = 24 * 60 * 60

type AuthAPI =
  "auth"
    :> ( "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] User
           :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
       )

authServer :: MyServer AuthAPI
authServer = register :<|> login

register :: RegisterRequest -> MyHandler User
register RegisterRequest {..} = do
  logInfo_ $ "Registering user: " <> registerUsername
  -- TODO: more robust validation?
  when (T.null registerUsername) $ throwError $ err400 {errBody = "Username cannot be empty"}
  when (T.length registerUsername > 50) $ throwError $ err400 {errBody = "Username is too long"}
  when (T.null registerPassword) $ throwError $ err400 {errBody = "Password cannot be empty"}
  when (T.length registerPassword < 6) $ throwError $ err400 {errBody = "Password must be at least 6 characters"}
  exists <- runDB $ D.checkUserExists registerUsername
  when exists $ throwError $ err409 {errBody = "User already exists"}
  mHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 registerPassword)
  case mHash of
    Nothing -> throwError $ err500 {errBody = "Failed to hash password"}
    Just hash -> do
      sUser <- runDB $ D.insertUser registerUsername (decodeUtf8 hash)
      pure $ schemaUserToApiUser sUser

-- | Issue a JWT access token for the given user
issueAccessToken :: S.User -> MyHandler T.Text
issueAccessToken sUser = do
  jwtSettings <- asks appJWTSettings
  now <- liftIO getCurrentTime
  let expiry = addUTCTime accessTokenLifetime now
      claims = AuthUser (S._userId sUser) (S._username sUser)
  eToken <- liftIO $ makeJWT claims jwtSettings (Just expiry)
  case eToken of
    Left _err -> throwError $ err500 {errBody = "Failed to generate access token"}
    Right tokenBS -> pure $ TL.toStrict $ TLE.decodeUtf8 tokenBS

login :: LoginRequest -> MyHandler LoginResponse
login LoginRequest {..} = do
  logInfo_ $ "Login attempt for user: " <> loginUsername
  mUser <- runDB $ D.getUserByUsername loginUsername
  case mUser of
    Nothing -> throwError $ err401 {errBody = "Invalid username or password"}
    Just sUser -> do
      let valid = validatePassword (encodeUtf8 $ S._passwordHash sUser) (encodeUtf8 loginPassword)
      if not valid
        then throwError $ err401 {errBody = "Invalid username or password"}
        else do
          accessTok <- issueAccessToken sUser
          pure $
            LoginResponse
              { accessToken = accessTok,
                loginUser = schemaUserToApiUser sUser
              }
