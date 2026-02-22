{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Jizhang.API.Types
  ( module Jizhang.API.Types.Auth,
    module Jizhang.API.Types.Group,
    module Jizhang.API.Types.Receipt,
    module Jizhang.API.Types.Record,
    module Jizhang.API.Types.Report,
    module Jizhang.API.Types.User,
    AppEnv (..),
    MyHandler (..),
    MyServer,
    runDB,
  )
where

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson.Types (emptyObject)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Beam.Postgres (Pg, runBeamPostgresDebug)
import Database.PostgreSQL.Simple (Connection)
import Jizhang.API.Types.Auth
import Jizhang.API.Types.Group
import Jizhang.API.Types.Receipt
import Jizhang.API.Types.Record
import Jizhang.API.Types.Report
import Jizhang.API.Types.User
import Log.Class (MonadLog)
import Log.Data (LogLevel (..))
import Log.Monad (LogT, getLoggerIO)
import Servant
import Servant.Auth.Server (JWTSettings)

-- | Application environment available to all handlers
data AppEnv = AppEnv
  { appConn :: !Connection,
    appJWTSettings :: !JWTSettings
  }

type MyServer k = ServerT k MyHandler

newtype MyHandler a = MyHandler
  { runMyHandler :: ReaderT AppEnv (LogT Handler) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError, MonadLog, MonadBase IO, MonadFail)

instance MonadBaseControl IO MyHandler where
  type StM MyHandler a = Either ServerError a
  liftBaseWith f = MyHandler $ liftBaseWith $ \runInBase -> f (runInBase . runMyHandler)
  restoreM = MyHandler . restoreM

-- | Run a database action in the MyHandler monad
runDB :: Pg a -> MyHandler a
runDB m = do
  conn <- asks appConn
  logger <- getLoggerIO
  t <- liftIO getCurrentTime
  liftIO $ runBeamPostgresDebug (\x -> logger t LogTrace (T.pack x) emptyObject) conn m
