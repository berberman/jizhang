{-# LANGUAGE OverloadedStrings #-}

module Jizhang.Database.Admin where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types (Only (..))
import Jizhang.Database.Schema

insertAdmin :: Text -> Text -> Pg Admin
insertAdmin username passwordHash = do
  newUUID <- liftIO nextRandom
  let admin = Admin newUUID username passwordHash
  runInsert (insert (_admins jizhangDb) $ insertValues [admin]) >> pure admin

getAdminByUsername :: Text -> Pg (Maybe Admin)
getAdminByUsername username = runSelectReturningOne $ select $ do
  admin <- all_ (_admins jizhangDb)
  guard_ (_adminUsername admin ==. val_ username)
  pure admin

getAdminById :: UUID -> Pg (Maybe Admin)
getAdminById adminId = runSelectReturningOne $ select $ do
  admin <- all_ (_admins jizhangDb)
  guard_ (_adminId admin ==. val_ adminId)
  pure admin

getAllAdmins :: Pg [Admin]
getAllAdmins = runSelectReturningList $ select $ all_ (_admins jizhangDb)

ensureAdminExists :: PG.Connection -> Text -> Text -> IO ()
ensureAdminExists conn username passwordHash = do
  existing <- PG.query conn "SELECT id FROM admins WHERE username = ?" (Only username) :: IO [Only UUID]
  case existing of
    [] -> do
      newUUID <- nextRandom
      _ <- PG.execute conn "INSERT INTO admins (id, username, password_hash) VALUES (?, ?, ?)" (newUUID, username, passwordHash)
      pure ()
    _ -> pure ()
