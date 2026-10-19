{-# LANGUAGE OverloadedStrings #-}

-- | Account normalization and bcrypt preparation shared by signup and provisioning.
-- Expensive hashing precedes persistence; callers decide registration policy.
module Jizhang.API.User.Account
  ( normalizeUsername,
    normalizeDisplayName,
    normalizeEmail,
    validateUsername,
    validatePasswordText,
    hashPasswordText,
    createUserAccount,
    prepareLocalUserAccount
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import qualified Data.ByteString.Char8 as LBS
import Data.Char (isAscii, isControl, isSpace, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Jizhang.API.App (MyHandler, runDBSensitive, runDBSensitiveTransaction)
import Jizhang.API.Internal.Error (textToLBS)
import qualified Jizhang.Database.User as D (checkEmailExists, checkUserExists, insertUser)
import qualified Jizhang.Database.Schema as S
import Servant (ServerError (errBody), err400, err409, err500, throwError)
import qualified Text.Email.Validate as Email

-- | Trim an optional @ and lowercase ASCII; the first character excludes dot and hyphen.
normalizeUsername :: Text -> MyHandler Text
normalizeUsername raw = do
  let stripped = T.strip raw
      name = T.map (\c -> if isAscii c then toLower c else c) $ maybe stripped id (T.stripPrefix "@" stripped)
      first c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'
      rest c = first c || c == '.' || c == '-'
  unless (T.length name >= 1 && T.length name <= 50 && maybe False (first . fst) (T.uncons name) && T.all rest name) $
    throwError $
      err400 {errBody = textToLBS "Username must use 1–50 letters, numbers, dots, underscores or hyphens"}
  pure name

-- | Trim labels while rejecting empty, overlong or control-containing display names.
normalizeDisplayName :: Text -> MyHandler Text
normalizeDisplayName raw = do
  let name = T.strip raw
  unless (not (T.null name) && T.length name <= 100 && not (T.any isControl name)) $
    throwError $
      err400 {errBody = textToLBS "Display name must contain 1–100 characters"}
  pure name

-- | Accept validated ASCII addresses and store them lowercase for identity matching.
normalizeEmail :: Text -> MyHandler Text
normalizeEmail raw = do
  let email = T.strip raw
  unless (T.all isAscii email && T.length email <= 254 && not (T.any (\c -> isSpace c || isControl c) email) && Email.isValid (T.encodeUtf8 email)) $
    throwError $
      err400 {errBody = "Enter a valid email address"}
  pure $ T.toLower email

-- | Enforce character minimum and bcrypt's UTF-8 byte limit without truncating input.
validatePasswordText :: Text -> MyHandler ()
validatePasswordText rawPassword = do
  when (T.null rawPassword) $ throwError $ err400 {errBody = "Password cannot be empty"}
  when (T.length rawPassword < 6) $ throwError $ err400 {errBody = "Password must be at least 6 characters"}
  -- Bcrypt truncates by bytes, so a character-only limit would allow distinct inputs to share a hash.
  when (LBS.length (T.encodeUtf8 rawPassword) > 72) $ throwError $ err400 {errBody = "Password must be at most 72 UTF-8 bytes"}

-- | Hash already-validated plaintext; hashing failure is an internal error.
hashPasswordText :: Text -> MyHandler Text
hashPasswordText rawPassword = do
  mHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (T.encodeUtf8 rawPassword)
  case mHash of
    Nothing -> throwError $ err500 {errBody = "Failed to hash password"}
    Just hash -> pure $ T.decodeUtf8 hash

-- | Operator provisioning deliberately bypasses public registration switches.
createUserAccount :: Text -> Text -> Text -> Text -> MyHandler S.User
createUserAccount rawUsername rawDisplayName rawEmail password = do
  (username, displayName, email, hash) <- prepareLocalUserAccount rawUsername rawDisplayName rawEmail password
  runDBSensitiveTransaction $ D.insertUser username displayName email (Just hash)

-- | Validate and hash outside the transaction; callers choose the signup policy.
prepareLocalUserAccount :: Text -> Text -> Text -> Text -> MyHandler (Text, Text, Text, Text)
prepareLocalUserAccount rawUsername rawDisplayName rawEmail password = do
  username <- normalizeUsername rawUsername
  displayName <- normalizeDisplayName rawDisplayName
  email <- normalizeEmail rawEmail
  validatePasswordText password
  -- These early conflict checks avoid wasted bcrypt work; they do not reserve either identifier.
  usernameExists <- runDBSensitive $ D.checkUserExists username
  when usernameExists $ throwError $ err409 {errBody = "Username is already in use"}
  emailExists <- runDBSensitive $ D.checkEmailExists email
  when emailExists $ throwError $ err409 {errBody = "Email is already in use"}
  hash <- hashPasswordText password
  pure (username, displayName, email, hash)

-- | Legacy length-only validation used for operator usernames, not user normalization.
validateUsername :: Text -> MyHandler ()
validateUsername u = do
  when (T.null u) $ throwError $ err400 {errBody = "Username cannot be empty"}
  when (T.length u > 50) $ throwError $ err400 {errBody = "Username is too long"}
