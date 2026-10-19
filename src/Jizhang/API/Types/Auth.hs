{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Password-authentication JSON bodies with literal, prefixed request keys.
-- Request decoders reject unknown keys but leave credential policy to handlers.
module Jizhang.API.Types.Auth where

import Data.Aeson (FromJSON (..), ToJSON, defaultOptions, genericParseJSON, rejectUnknownFields)
import Data.Data (Typeable)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jizhang.API.Types.User

-- | All four fields are required strings; no credential or email validation runs
-- in this codec. Passwords are sensitive even though a ToJSON instance exists.
data RegisterRequest = RegisterRequest
  { -- | Requested public handle, distinct from the account email.
    registerUsername :: !Text,
    -- | Requested public name shown with ledger activity.
    registerDisplayName :: !Text,
    -- | Private account contact address.
    registerEmail :: !Text,
    -- | Plaintext credential submitted for registration; never log this body.
    registerPassword :: !Text
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON RegisterRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Required loginIdentifier and loginPassword strings, without aliases or extra
-- fields. Decoding does not authenticate the account or normalize the identifier.
data LoginRequest = LoginRequest
  { -- | Account identifier interpreted by the login handler.
    loginIdentifier :: !Text,
    -- | Plaintext submitted credential; the request deliberately has no Show.
    loginPassword :: !Text
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON LoginRequest where
  parseJSON = genericParseJSON defaultOptions {rejectUnknownFields = True}

-- | Bearer credential and public user projection. The JSON contains accessToken
-- and loginUser, with no private email or provider identity details.
-- Both fields are required and non-null; response decoding ignores extra keys.
data LoginResponse = LoginResponse
  { -- | Sensitive bearer token; possession authorizes its use.
    accessToken :: !Text,
    -- | Public account identity associated with the login.
    loginUser :: !User
  }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
