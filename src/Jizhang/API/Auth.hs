{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Public local signup and password login. Credentials use unlogged persistence;
-- session creation rechecks the verified account snapshot before committing.
module Jizhang.API.Auth
  ( AuthAPI,
    authServer,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Crypto.BCrypt (validatePassword)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Jizhang.API.App
import Jizhang.API.RequestProtection (enforceUserCredentialLimit)
import Jizhang.API.Types
import Jizhang.API.User.Account (normalizeUsername, normalizeEmail, prepareLocalUserAccount)
import Jizhang.API.User.Projection (schemaUserToApiUser)
import Jizhang.Auth.Session (SessionAuthentication (PasswordSession), prepareUserSession)
import qualified Jizhang.Database as D
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import Log.Class
import Servant

-- | Registration returns a public user, while login returns a persisted-session token.
type AuthAPI =
  "auth"
    :> ( "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] User
           :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
       )

authServer :: MyServer AuthAPI
authServer = register :<|> login

-- | Prepare credentials before atomically checking whether local signup is enabled.
register :: RegisterRequest -> MyHandler User
register RegisterRequest {..} = do
  (username, displayName, email, hash) <- prepareLocalUserAccount registerUsername registerDisplayName registerEmail registerPassword
  -- Signup policy can change while bcrypt runs; decide it with the insert, not before hashing.
  result <- runDBSensitiveTransaction $ do
    settings <- A.getAuthSettings
    if S._settingsLocalRegistrationEnabled settings
      then Just <$> D.insertUser username displayName email (Just hash)
      else pure Nothing
  maybe (throwError $ err403 {errBody = "Local registration is disabled"}) (pure . schemaUserToApiUser) result

-- | Normalize identifier failures to the same 401 as bad credentials; rate-limit
-- by resolved account or identifier digest before attempting password verification.
login :: LoginRequest -> MyHandler LoginResponse
login LoginRequest {..} = do
  logInfo_ "Local password login attempt"
  let invalid = err401 {errBody = "Invalid username/email or password"}
      raw = T.strip loginIdentifier
      isEmail = T.any (== '@') (T.drop 1 raw)
      normalize = if isEmail then normalizeEmail raw else normalizeUsername raw
  identifier <- normalize `catchError` (\err -> if errHTTPCode err == 400 then throwError invalid else throwError err)
  mUser <- runDBSensitive $ if isEmail then D.getUserByEmail identifier else D.getUserByUsername identifier
  enforceUserCredentialLimit (S._userId <$> mUser) identifier
  case mUser of
    Just sUser
      | S._enabled sUser,
        BS.length (encodeUtf8 loginPassword) <= 72,
        Just hash <- S._passwordHash sUser,
        validatePassword (encodeUtf8 hash) (encodeUtf8 loginPassword) -> do
          -- Bcrypt runs without holding a transaction; its result proves only this account snapshot.
          (session, response) <- prepareUserSession sUser PasswordSession
          runDBSensitiveTransaction $ do
            current <- D.getUserById (S._userId sUser)
            -- A concurrent password reset or disable must win over this already-verified password.
            case current of
              Just user -> do
                unless
                  ( S._enabled user
                      && S._passwordHash user == S._passwordHash sUser
                      && S._authInvalidatedAt user == S._authInvalidatedAt sUser
                  )
                  A.abortAuthTransaction
                A.insertUserSession session
              Nothing -> A.abortAuthTransaction
          -- Keep the signed token private until its revocable session row has committed.
          pure response
    _ -> throwError invalid
