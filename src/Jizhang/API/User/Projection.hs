{-# LANGUAGE OverloadedStrings #-}

-- | Public user views and historical participant lookup. Shared ledger views expose
-- handles and names; only the profile view includes email and verification state.
module Jizhang.API.User.Projection
  ( UserMap,
    schemaUserToApiUser,
    schemaUserToProfile,
    resolveUser,
    storedUser
  )
where

import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import Jizhang.API.Types.User (User (User), UserId (UserId), UserProfile (UserProfile))
import qualified Jizhang.Database.Schema as S

-- | UUID-keyed users let ledger history survive changes to handles and display names.
type UserMap = M.Map UUID User

-- | Omit email, credentials and authentication state from shared participant views.
schemaUserToApiUser :: S.User -> User
schemaUserToApiUser u = User (coerce $ S._userId u) (S._username u) (S._displayName u)

-- | Add account email metadata for callers allowed to view the full profile.
schemaUserToProfile :: S.User -> UserProfile
schemaUserToProfile u = UserProfile (schemaUserToApiUser u) (S._email u) (S._emailVerified u)

-- | Partial lookup: callers must supply a map containing this historical user.
resolveUser :: UserMap -> UUID -> User
resolveUser um uid = um M.! uid

-- | Report a missing historical participant as stored-data corruption.
storedUser :: UserMap -> UUID -> Either Text User
storedUser users uid = maybe (Left "Stored user is missing from group history") Right (M.lookup uid users)
