-- | Account rows shared by ledger and authentication code. Lookups include
-- disabled accounts; authorization and transaction boundaries belong to callers.
module Jizhang.Database.User where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import Jizhang.Database.Schema

-- | Create an enabled, unverified account. The optional hash must already be
-- derived by the caller; this function does not normalize or reserve identifiers.
insertUser :: Username -> Text -> Text -> Maybe Text -> Pg User
insertUser username displayName email passwordHash = do
  newUUID <- liftIO nextRandom
  let user = User newUUID username displayName email False passwordHash True Nothing
  insertUserRow user
  pure user

-- | Insert a complete account snapshot, preserving its authentication state.
-- Uniqueness and foreign-key failures propagate from PostgreSQL.
insertUserRow :: User -> Pg ()
insertUserRow user = runInsert (insert (_users jizhangDb) $ insertValues [user])

deleteUser :: UUID -> Pg ()
deleteUser uid = runDelete $ delete (_users jizhangDb) (\user -> _userId user ==. val_ uid)

checkUserExists :: Username -> Pg Bool
checkUserExists username =
  isJust
    <$> runSelectReturningOne
      ( select $ do
          user <- all_ (_users jizhangDb)
          guard_ (_username user ==. val_ username)
          pure (_userId user)
      )

getUserByUsername :: Username -> Pg (Maybe User)
getUserByUsername username = runSelectReturningOne $ select $ do
  user <- all_ (_users jizhangDb)
  guard_ (_username user ==. val_ username)
  pure user

getUserById :: UUID -> Pg (Maybe User)
getUserById uid = runSelectReturningOne $ select $ do
  user <- all_ (_users jizhangDb)
  guard_ (_userId user ==. val_ uid)
  pure user

getUserByEmail :: Text -> Pg (Maybe User)
getUserByEmail email = runSelectReturningOne $ select $ do
  user <- all_ (_users jizhangDb)
  guard_ (_email user ==. val_ email)
  pure user

checkEmailExists :: Text -> Pg Bool
checkEmailExists email = isJust <$> getUserByEmail email

-- | Change public names only, leaving credentials and invalidation state intact.
-- A missing UUID is a no-op; callers must check conflicts and permissions.
updateUserProfile :: UUID -> Text -> Text -> Pg ()
updateUserProfile uid username displayName =
  runUpdate $
    update
      (_users jizhangDb)
      (\user -> mconcat [_username user <-. val_ username, _displayName user <-. val_ displayName])
      (\user -> _userId user ==. val_ uid)

getAllUsers :: Pg [User]
getAllUsers = runSelectReturningList $ select $ all_ (_users jizhangDb)

-- | Public directory projection, excluding email and authentication columns.
-- No enabled-account filter or ordering is applied.
getUserDirectory :: Pg [(UUID, Text, Text)]
getUserDirectory = runSelectReturningList $ select $ do
  user <- all_ (_users jizhangDb)
  pure (_userId user, _username user, _displayName user)

-- | Get a map from user UUID to username for resolving FKs
getAllUsersMap :: Pg (M.Map UUID Username)
getAllUsersMap = do
  users <- runSelectReturningList $ select $ all_ (_users jizhangDb)
  pure $ M.fromList [(_userId u, _username u) | u <- users]
