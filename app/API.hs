{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Control.Monad (unless, when)
import Control.Monad.Reader
import qualified DB as D
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.Int (Int8)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import MyUUID (MyUUID)
import qualified Schema as S
import Servant

newtype User = User Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

instance FromHttpApiData User where
  parseUrlPiece = Right . User

newtype GroupId = GroupId MyUUID
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema, ToParamSchema)

newtype RecordId = RecordId MyUUID
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema, ToParamSchema)

data Group = Group
  { groupId :: GroupId,
    groupName :: Text,
    members :: [User]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

data RecordSplit = RecordSplit
  { username :: User,
    percentage :: Int8,
    splitAmount :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

data Record
  = ExpenseRecord
      { recordId :: RecordId,
        title :: Text,
        amount :: Double,
        byUsername :: User,
        at :: UTCTime,
        groupId :: GroupId,
        splits :: [RecordSplit]
      }
  | TransferRecord
      { recordId :: RecordId,
        title :: Text,
        amount :: Double,
        byUsername :: User,
        toUsername :: User,
        at :: UTCTime,
        groupId :: GroupId
      }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

data BalanceBreakdown = BalanceBreakdown
  { recordId :: RecordId,
    amount :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

data Balance = Balance
  { username :: User,
    groupId :: GroupId,
    totalAmount :: Double,
    breakdown :: [BalanceBreakdown]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

data Settlement = Settlement
  { fromUsername :: User,
    toUsername :: User,
    groupId :: GroupId,
    amount :: Double
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Typeable, ToSchema)

type UserAPI =
  -- Get all users
  "users" :> Get '[JSON] [User]
    -- Create a new user
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
    -- Delete a user
    :<|> "users" :> Capture "username" User :> DeleteNoContent
    -- Get a specific user
    :<|> "users" :> Capture "username" User :> Get '[JSON] User
    -- Get groups for a specific user
    :<|> "users" :> Capture "username" User :> "groups" :> Get '[JSON] [Group]

type GroupAPI =
  -- Get all groups
  "groups" :> Get '[JSON] [Group]
    -- Create a new group
    :<|> "groups" :> ReqBody '[PlainText] Text :> Post '[JSON] Group
    -- Get a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> Get '[JSON] Group
    -- Update a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> ReqBody '[PlainText] Text :> Put '[JSON] Group
    -- Delete a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> DeleteNoContent
    -- Add a member to a group
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> ReqBody '[JSON] User :> Post '[JSON] Group
    -- Delete a member from a group
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> Capture "username" User :> DeleteNoContent

type MyServer k = ServerT k (ReaderT Connection Handler)

runDB :: SqliteM a -> ReaderT Connection Handler a
runDB m = ask >>= \conn -> liftIO $ runBeamSqliteDebug putStrLn conn m

ensureUserExists :: Text -> ReaderT Connection Handler ()
ensureUserExists u = do
  exists <- runDB $ D.checkUserExists u
  unless exists $ throwError $ err404 {errBody = "User not found"}

ensureGroupExists :: MyUUID -> ReaderT Connection Handler ()
ensureGroupExists gId = do
  exists <- runDB $ D.checkGroupExists gId
  unless exists $ throwError $ err404 {errBody = "Group not found"}

userServer :: MyServer UserAPI
userServer = getUsers :<|> createUser :<|> deleteUser :<|> getUser :<|> getGroupsForUser
  where
    getUsers = do
      users <- runDB D.getAllUsers
      pure $ coerce <$> users
    createUser (User u) = do
      exists <- runDB $ D.checkUserExists u
      _ <-
        if exists
          then throwError $ err409 {errBody = "User already exists"}
          else runDB $ D.insertUser u
      pure $ User u
    deleteUser (User u) = do
      ensureUserExists u
      runDB $ D.deleteUser u
      pure NoContent
    getUser (User u) = do
      ensureUserExists u
      pure $ User u
    getGroupsForUser (User u) = do
      ensureUserExists u
      groups <- runDB $ D.getGroupsForUser u
      gms <-
        mapM
          ( \g ->
              let gid = S._groupId g
                  gname = S._groupName g
               in (gid,gname,) <$> runDB (D.getAllMembersInGroup gid)
          )
          groups
      pure [Group (coerce gid) gname (coerce . S.unUserId . S._gmUser <$> uss) | (gid, gname, uss) <- gms]

groupServer :: MyServer GroupAPI
groupServer = getAllGroups :<|> createGroup :<|> getGroup True :<|> updateGroup :<|> deleteGroup :<|> addGroupMember :<|> deleteGroupMember
  where
    getAllGroups = do
      gps <- runDB D.getAllGroupWithMembers
      let mp = M.fromListWith (++) [((S._groupId g, S._groupName g), [S.unUserId $ S._gmUser m]) | (g, m) <- gps]
      pure [Group (coerce gid) gn (coerce <$> uss) | ((gid, gn), uss) <- M.toList mp]
    createGroup gname = do
      group <- runDB $ D.insertGroup gname
      pure $ Group (coerce $ S._groupId group) (S._groupName group) [] -- No members initially
    getGroup checkExistence (GroupId gId) = do
      when checkExistence $ ensureGroupExists gId
      (g, ms) <- fmap fromJust <$> runDB $ D.getGroupWithMembers gId
      pure $ Group (coerce $ S._groupId g) (S._groupName g) (coerce . S.unUserId . S._gmUser <$> ms)
    updateGroup (GroupId gId) newName = do
      ensureGroupExists gId
      runDB $ D.updateGroup gId (Just newName)
      getGroup False (GroupId gId)
    deleteGroup (GroupId gId) = do
      ensureGroupExists gId
      _ <- runDB $ D.deleteGroup gId
      pure NoContent
    addGroupMember (GroupId gId) (User u) = do
      ensureGroupExists gId
      ensureUserExists u
      _ <- runDB $ D.addGroupMember u gId
      getGroup False (GroupId gId)
    deleteGroupMember (GroupId gId) (User u) = do
      ensureGroupExists gId
      isMember <- runDB $ D.isUserInGroup u gId
      if not isMember
        then throwError $ err404 {errBody = "User is not a member of the group"}
        else do
          _ <- runDB $ D.deleteGroupMember u gId
          pure NoContent

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Connection -> Application
app conn = serve userAPI $ hoistServer userAPI (`runReaderT` conn) userServer
