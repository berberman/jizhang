module Jizhang.Database.User where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import Jizhang.Database.Schema

insertUser :: Username -> Text -> Pg User
insertUser username passwordHash = do
  newUUID <- liftIO nextRandom
  let user = User newUUID username passwordHash
  runInsert (insert (_users jizhangDb) $ insertValues [user]) >> pure user

deleteUser :: UUID -> Pg ()
deleteUser uid = runDelete $ delete (_users jizhangDb) (\user -> _userId user ==. val_ uid)

checkUserExists :: Username -> Pg Bool
checkUserExists username = do
  users <- runSelectReturningList $ select $ do
    user <- all_ (_users jizhangDb)
    guard_ (_username user ==. val_ username)
    pure user
  pure $ not (null users)

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

getAllUsers :: Pg [User]
getAllUsers = runSelectReturningList $ select $ all_ (_users jizhangDb)

-- | Get a map from user UUID to username for resolving FKs
getAllUsersMap :: Pg (M.Map UUID Username)
getAllUsersMap = do
  users <- runSelectReturningList $ select $ all_ (_users jizhangDb)
  pure $ M.fromList [(_userId u, _username u) | u <- users]
