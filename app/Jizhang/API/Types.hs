{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Jizhang.API.Types where

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (emptyObject)
import Data.Data (Typeable)
import Data.Int (Int8)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, getCurrentTime)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Jizhang.Common.MyUUID (MyUUID)
import Log.Class (MonadLog)
import Log.Data (LogLevel (..))
import Log.Monad (LogT, getLoggerIO)
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
  { groupId :: !GroupId,
    groupName :: !Text,
    members :: ![User]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecordSplit = RecordSplit
  { username :: !User,
    share :: !Int8,
    splitAmount :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecordSplitRequest = RecordSplitRequest
  { username :: !User,
    share :: !Int8
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExpenseRecordRequest = ExpenseRecordRequest
  { title :: !Text,
    amount :: !Double,
    byUsername :: !User,
    date :: !Day,
    splits :: ![RecordSplitRequest]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransferRecordRequest = TransferRecordRequest
  { amount :: !Double,
    byUsername :: !User,
    toUsername :: !User,
    date :: !Day
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Record
  = ExpenseRecord
      { recordId :: !RecordId,
        title :: !Text,
        amount :: !Double,
        byUsername :: !User,
        date :: !Day,
        groupId :: !GroupId,
        splits :: ![RecordSplit]
      }
  | TransferRecord
      { recordId :: !RecordId,
        title :: !Text,
        amount :: !Double,
        byUsername :: !User,
        toUsername :: !User,
        date :: !Day,
        groupId :: !GroupId
      }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BalanceBreakdown = BalanceBreakdown
  { recordId :: !RecordId,
    title :: !Text,
    amount :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Balance = Balance
  { username :: !User,
    totalAmount :: !Double,
    breakdown :: ![BalanceBreakdown]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Settlement = Settlement
  { fromUsername :: !User,
    toUsername :: !User,
    amount :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Report = Report
  { groupId :: !GroupId,
    balances :: ![Balance],
    settlements :: ![Settlement]
  }
  deriving stock (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
