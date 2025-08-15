{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified DB as D
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Char8 as LBS
import Data.ByteString.Lazy (LazyByteString)
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.Int (Int8)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, maybeToList)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Log (LogLevel (LogTrace), logInfo_)
import Log.Class (MonadLog)
import Log.Monad
import MyUUID (MyUUID, uuidToText)
import qualified Schema as S
import Servant

newtype User = User Text
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData User where
  parseUrlPiece = Right . User

newtype GroupId = GroupId MyUUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData)

newtype RecordId = RecordId MyUUID
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData)

data Group = Group
  { groupId :: GroupId,
    groupName :: Text,
    members :: [User]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecordSplit = RecordSplit
  { username :: User,
    percentage :: Int8,
    splitAmount :: Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecordSplitRequest = RecordSplitRequest
  { username :: User,
    percentage :: Int8
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExpenseRecordRequest = ExpenseRecordRequest
  { title :: Text,
    amount :: Double,
    byUsername :: User,
    at :: UTCTime,
    splits :: [RecordSplitRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransferRecordRequest = TransferRecordRequest
  { amount :: Double,
    byUsername :: User,
    toUsername :: User,
    at :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BalanceBreakdown = BalanceBreakdown
  { recordId :: RecordId,
    amount :: Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Balance = Balance
  { username :: User,
    groupId :: GroupId,
    totalAmount :: Double,
    breakdown :: [BalanceBreakdown]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Settlement = Settlement
  { fromUsername :: User,
    toUsername :: User,
    groupId :: GroupId,
    amount :: Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
    :<|> "groups" :> ReqBody '[JSON] Text :> Post '[JSON] Group
    -- Get a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> Get '[JSON] Group
    -- Update a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> ReqBody '[JSON] Text :> Put '[JSON] Group
    -- Delete a specific group
    :<|> "groups" :> Capture "groupId" GroupId :> DeleteNoContent
    -- Add a member to a group
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> ReqBody '[JSON] User :> Post '[JSON] Group
    -- Delete a member from a group
    :<|> "groups" :> Capture "groupId" GroupId :> "members" :> Capture "username" User :> DeleteNoContent

type RecordAPI =
  -- Add a new expense record to a group
  "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> ReqBody '[JSON] ExpenseRecordRequest :> Post '[JSON] Record
    -- Add a new transfer record to a group
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> ReqBody '[JSON] TransferRecordRequest :> Post '[JSON] Record
    -- Get a specific record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> Get '[JSON] Record
    -- Get all records in a group
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Get '[JSON] [Record]
    -- Delete a specific record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> Capture "recordId" RecordId :> DeleteNoContent
    -- Update a specific transfer record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "transfer" :> Capture "recordId" RecordId :> ReqBody '[JSON] TransferRecordRequest :> Put '[JSON] Record
    -- Update a specific expense record
    :<|> "groups" :> Capture "groupId" GroupId :> "records" :> "expense" :> Capture "recordId" RecordId :> ReqBody '[JSON] ExpenseRecordRequest :> Put '[JSON] Record

type ReportAPI =
  "groups" :> Capture "groupId" GroupId :> "balances" :> Get '[JSON] [Balance]
    :<|> "groups" :> Capture "groupId" GroupId :> "settle" :> Get '[JSON] [Settlement]
    :<|> "groups" :> Capture "groupId" GroupId :> "settle" :> Capture "username" User :> Get '[JSON] [Settlement]

type MyServer k = ServerT k MyHandler

newtype MyHandler a = MyHandler
  { runMyHandler :: ReaderT Connection (LogT Handler) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Connection, MonadError ServerError, MonadLog, MonadBase IO)

instance MonadBaseControl IO MyHandler where
  type StM MyHandler a = Either ServerError a
  liftBaseWith f = MyHandler $ liftBaseWith $ \runInBase -> f (runInBase . runMyHandler)
  restoreM = MyHandler . restoreM

runDB :: SqliteM a -> MyHandler a
runDB m = do
  conn <- ask
  logger <- getLoggerIO
  t <- liftIO getCurrentTime
  liftIO $ runBeamSqliteDebug (\x -> logger t LogTrace (T.pack x) emptyObject) conn m

textToLBS :: Text -> LazyByteString
textToLBS = LBS.fromStrict . T.encodeUtf8

ensureUserExists :: Text -> MyHandler ()
ensureUserExists u = do
  exists <- runDB $ D.checkUserExists u
  unless exists $ throwError $ err404 {errBody = "User " <> textToLBS u <> " not found"}

ensureGroupExists :: MyUUID -> MyHandler ()
ensureGroupExists gId = do
  exists <- runDB $ D.checkGroupExists gId
  unless exists $ throwError $ err404 {errBody = "Group with ID " <> textToLBS (uuidToText gId) <> " not found"}

validateUsername :: Text -> MyHandler ()
validateUsername u = do
  when (T.null u) $ throwError $ err400 {errBody = "Username cannot be empty"}
  when (T.length u > 50) $ throwError $ err400 {errBody = "Username is too long"}

validateGroupName :: Text -> MyHandler ()
validateGroupName gname = do
  when (T.null gname) $ throwError $ err400 {errBody = "Group name cannot be empty"}
  when (T.length gname > 100) $ throwError $ err400 {errBody = "Group name is too long"}

userServer :: MyServer UserAPI
userServer =
  getUsers
    :<|> createUser
    :<|> deleteUser
    :<|> getUser
    :<|> getGroupsForUser
  where
    getUsers = do
      logInfo_ "Fetching all users"
      users <- runDB D.getAllUsers
      pure $ coerce <$> users
    createUser (User u) = do
      logInfo_ $ "Creating user: " <> u
      validateUsername u
      exists <- runDB $ D.checkUserExists u
      _ <-
        if exists
          then throwError $ err409 {errBody = "User already exists"}
          else runDB $ D.insertUser u
      pure $ User u
    deleteUser (User u) = do
      logInfo_ $ "Deleting user: " <> u
      ensureUserExists u
      runDB $ D.deleteUser u
      pure NoContent
    getUser (User u) = do
      logInfo_ $ "Fetching user: " <> u
      ensureUserExists u
      pure $ User u
    getGroupsForUser (User u) = do
      logInfo_ $ "Fetching groups for user: " <> u
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
groupServer =
  getAllGroups
    :<|> createGroup
    :<|> getGroup True
    :<|> updateGroup
    :<|> deleteGroup
    :<|> addGroupMember
    :<|> deleteGroupMember
  where
    getAllGroups = do
      logInfo_ "Fetching all groups with members"
      gps <- runDB D.getAllGroupWithMembers
      let mp = M.fromListWith (++) [((S._groupId g, S._groupName g), [S.unUserId $ S._gmUser m' | m' <- maybeToList m]) | (g, m) <- gps]
      pure [Group (coerce gid) gn (coerce <$> uss) | ((gid, gn), uss) <- M.toList mp]
    createGroup gname = do
      logInfo_ $ "Creating group: " <> gname
      validateGroupName gname
      group <- runDB $ D.insertGroup gname
      pure $ Group (coerce $ S._groupId group) (S._groupName group) [] -- No members initially
    getGroup checkExistence (GroupId gId) = do
      logInfo_ $ "Fetching group with ID: " <> T.pack (show gId)
      when checkExistence $ ensureGroupExists gId
      (g, ms) <- fmap fromJust <$> runDB $ D.getGroupWithMembers gId
      pure $ Group (coerce $ S._groupId g) (S._groupName g) (coerce . S.unUserId . S._gmUser <$> ms)
    updateGroup (GroupId gId) newName = do
      logInfo_ $ "Updating group " <> uuidToText gId <> " to new name: " <> newName
      ensureGroupExists gId
      validateGroupName newName
      runDB $ D.updateGroup gId (Just newName)
      getGroup False (GroupId gId)
    deleteGroup (GroupId gId) = do
      logInfo_ $ "Deleting group with ID: " <> uuidToText gId
      ensureGroupExists gId
      _ <- runDB $ D.deleteGroup gId
      pure NoContent
    addGroupMember (GroupId gId) (User u) = do
      logInfo_ $ "Adding user " <> u <> " to group with ID: " <> uuidToText gId
      ensureGroupExists gId
      ensureUserExists u
      _ <- runDB $ D.addGroupMember u gId
      getGroup False (GroupId gId)
    deleteGroupMember (GroupId gId) (User u) = do
      logInfo_ $ "Removing user " <> u <> " from group with ID: " <> uuidToText gId
      ensureGroupExists gId
      isMember <- runDB $ D.isUserInGroup u gId
      if not isMember
        then throwError $ err404 {errBody = "User is not a member of the group"}
        else do
          _ <- runDB $ D.deleteGroupMember u gId
          pure NoContent

validateExpenseRecordRequest :: ExpenseRecordRequest -> MyHandler ()
validateExpenseRecordRequest ExpenseRecordRequest {..} = do
  when (T.null title) $ throwError $ err400 {errBody = "Title cannot be empty"}
  when (amount <= 0) $ throwError $ err400 {errBody = "Amount must be greater than zero"}
  when (sum ([percentage | RecordSplitRequest {..} <- splits]) /= 100) $
    throwError $
      err400 {errBody = "Total percentage of splits must equal 100"}
  ensureUserExists (coerce byUsername)
  forM_ splits $ \RecordSplitRequest {..} -> do
    ensureUserExists (coerce username)
    when (percentage < 0 || percentage > 100) $
      throwError $
        err400 {errBody = "Percentage must be between 0 and 100"}

validateTransferRecordRequest :: TransferRecordRequest -> MyHandler ()
validateTransferRecordRequest TransferRecordRequest {..} = do
  when (amount <= 0) $ throwError $ err400 {errBody = "Amount must be greater than zero"}
  ensureUserExists (coerce byUsername)
  ensureUserExists (coerce toUsername)
  when (byUsername == toUsername) $
    throwError $
      err400 {errBody = "Transfer cannot be made to the same user"}

recordToExpenseRecord :: S.Record -> [S.RecordSplit] -> Record
recordToExpenseRecord record ssplits =
  let recordId = coerce $ S._recordId record
      title = S._title record
      amount = S._amount record
      byUsername = coerce . S.unUserId $ S._paidBy record
      at = S._createdAt record
      groupId = coerce . S.unGroupId $ S._recordGroup record
      splits = [RecordSplit (coerce $ S.unUserId _rsUser) _percentage _splitAmount | S.RecordSplit {..} <- ssplits]
   in ExpenseRecord {..}

recordToTransferRecord :: S.Record -> Record
recordToTransferRecord record =
  let recordId = coerce $ S._recordId record
      title = S._title record
      amount = S._amount record
      byUsername = coerce . S.unUserId $ S._paidBy record
      toUsername = coerce . fromJust . S.unUserId $ S._transferTo record
      at = S._createdAt record
      groupId = coerce . S.unGroupId $ S._recordGroup record
   in TransferRecord {..}

ensureRecordExists :: MyUUID -> MyHandler ()
ensureRecordExists rId = do
  exists <- runDB $ D.checkRecordExists rId
  unless exists $ throwError $ err404 {errBody = "Record with ID " <> textToLBS (uuidToText rId) <> " not found"}

recordServer :: MyServer RecordAPI
recordServer =
  addExpenseRecord
    :<|> addTransferRecord
    :<|> getRecord
    :<|> getRecordsInGroup
    :<|> deleteRecord
    :<|> updateTransfer
    :<|> updateExpense
  where
    addExpenseRecord (GroupId gId) req@ExpenseRecordRequest {..} = do
      logInfo_ $ "Adding expense record to group " <> uuidToText gId
      ensureGroupExists gId
      validateExpenseRecordRequest req
      -- Add the record
      record <- runDB $ D.insertRecord title amount (coerce byUsername) Nothing gId at
      -- Add the splits with split amounts calculated
      ss <- forM splits $ \RecordSplitRequest {..} ->
        runDB $ D.insertRecordSplit (S._recordId record) (coerce username) percentage (fromIntegral percentage * amount)
      pure $ recordToExpenseRecord record ss

    addTransferRecord (GroupId gId) req@TransferRecordRequest {..} = do
      logInfo_ $ "Adding transfer record to group " <> uuidToText gId
      ensureGroupExists gId
      validateTransferRecordRequest req
      record <- runDB $ D.insertRecord "Transfer" amount (coerce byUsername) (Just (coerce toUsername)) gId at
      pure $ recordToTransferRecord record
    getRecord (GroupId gId) (RecordId rId) = do
      -- We actually don't need group to get the record, but we still check it exists
      logInfo_ $ "Fetching record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
      ensureGroupExists gId
      ensureRecordExists rId
      (record, ssplits) <- fromJust <$> runDB (D.getRecordWithSplits rId)
      pure $
        if S.isTransferRecord record
          then recordToTransferRecord record
          else recordToExpenseRecord record ssplits
    getRecordsInGroup (GroupId gId) = do
      logInfo_ $ "Fetching all records in group " <> uuidToText gId
      ensureGroupExists gId
      rs <- runDB $ D.getRecordsWithSplitsForGroup gId
      let mp = M.fromListWith (++) [(r, maybeToList s) | (r, s) <- rs]
      pure [if S.isTransferRecord r then recordToTransferRecord r else recordToExpenseRecord r ss | (r, ss) <- M.toList mp]
    deleteRecord (GroupId gId) (RecordId rId) = do
      -- Again we don't need group to delete the record, but we still check it exists
      logInfo_ $ "Deleting record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
      ensureGroupExists gId
      ensureRecordExists rId
      runDB $ D.deleteRecord rId
      pure NoContent
    updateTransfer (GroupId gId) (RecordId rId) req@TransferRecordRequest {..} = do
      logInfo_ $ "Updating transfer record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
      old <- getRecord (GroupId gId) (RecordId rId)
      case old of
        TransferRecord {} -> do
          validateTransferRecordRequest req
          _ <- runDB $ D.updateRecord rId Nothing (Just amount) (Just $ coerce byUsername) (Just $ Just $ coerce toUsername) (Just at)
          getRecord (GroupId gId) (RecordId rId)
        _ -> throwError $ err400 {errBody = "Record is not a transfer record"}
    updateExpense (GroupId gId) (RecordId rId) req@ExpenseRecordRequest {..} = do
      logInfo_ $ "Updating expense record with ID " <> uuidToText rId <> " in group " <> uuidToText gId
      old <- getRecord (GroupId gId) (RecordId rId)
      case old of
        ExpenseRecord {recordId = rid} -> do
          validateExpenseRecordRequest req
          -- Delete existing splits
          runDB $ D.deleteRecordSplitsForRecord rId
          -- Update the record
          runDB $ D.updateRecord rId (Just title) (Just amount) (Just $ coerce byUsername) Nothing (Just at)
          -- Add new splits with split amounts calculated
          forM_ splits $ \RecordSplitRequest {..} ->
            runDB $ D.insertRecordSplit (coerce rid) (coerce username) percentage (fromIntegral percentage * amount)
          -- Return the updated record
          getRecord (GroupId gId) (RecordId rId)
        _ -> throwError $ err400 {errBody = "Record is not an expense record"}

jizhangServer :: MyServer JizhangAPI
jizhangServer = userServer :<|> groupServer :<|> recordServer

type JizhangAPI = UserAPI :<|> GroupAPI :<|> RecordAPI

jizhangAPI :: Proxy JizhangAPI
jizhangAPI = Proxy

app :: Connection -> Logger -> Application
app conn logger =
  serve jizhangAPI $
    hoistServer
      jizhangAPI
      ( runLogT
          "jizhang"
          logger
          maxBound
          . logExceptions
          . flip runReaderT conn
          . runMyHandler
      )
      jizhangServer
