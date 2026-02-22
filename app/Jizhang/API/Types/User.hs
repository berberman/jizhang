{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jizhang.API.Types.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Auth.JWT (FromJWT, ToJWT)

-- | User IDs (UUID)
newtype UserId = UserId UUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

-- | Usernames in request types and URL captures
newtype Username = Username Text
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData Username where
  parseUrlPiece = Right . Username

instance ToHttpApiData Username where
  toUrlPiece (Username u) = u

-- | Rich user type for API responses
data User = User
  { userId :: !UserId,
    username :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Authenticated user extracted from JWT claims
data AuthUser = AuthUser
  { authUserId :: !UUID,
    authUsername :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToJWT, FromJWT)
