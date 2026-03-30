{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jizhang.API.Types.Admin where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant.Auth.JWT (FromJWT, ToJWT)

data AdminBootstrap = AdminBootstrap
  { adminBootstrapUsername :: !Text,
    adminBootstrapPassword :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdminLoginRequest = AdminLoginRequest
  { username :: !Text,
    password :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdminLoginResponse = AdminLoginResponse
  { accessToken :: !Text,
    adminUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdminSummary = AdminSummary
  { adminId :: !UUID,
    summaryUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdminCreateUserRequest = AdminCreateUserRequest
  { createUsername :: !Text,
    createPassword :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdminCreateAdminRequest = AdminCreateAdminRequest
  { createAdminUsername :: !Text,
    createAdminPassword :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuthAdmin = AuthAdmin
  { authAdminId :: !UUID,
    authAdminUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToJWT, FromJWT)
