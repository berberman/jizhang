{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Jizhang.API where

import Control.Monad.Reader (ReaderT (..))
import Data.Swagger (Swagger)
import Jizhang.API.Auth
import Jizhang.API.Group
import Jizhang.API.Import
import Jizhang.API.Receipt
import Jizhang.API.Record
import Jizhang.API.Report
import Jizhang.API.Types
import Jizhang.API.User
import Log (LogT, LoggerEnv (leLogger), MonadLog (getLoggerEnv), logExceptions, runLogT)
import Network.Wai.Log (mkLogMiddleware)
import Servant
import Servant.Auth.Server
import Servant.Auth.Swagger ()
import Servant.Swagger

jizhangServer :: AuthUser -> MyServer JizhangAPI
jizhangServer authUser =
  userServer authUser
    :<|> groupServer authUser
    :<|> recordServer authUser
    :<|> reportServer authUser
    :<|> importServer authUser
    :<|> receiptServer authUser

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type JizhangAPI = UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ImportAPI :<|> ReceiptAPI

type ProtectedAPI = Auth '[JWT] AuthUser :> JizhangAPI

type API = AuthAPI :<|> ProtectedAPI :<|> SwaggerAPI

swaggerServer :: MyServer SwaggerAPI
swaggerServer = pure $ toSwagger (Proxy :: Proxy (AuthAPI :<|> ProtectedAPI))

protectedServer :: AuthResult AuthUser -> ServerT JizhangAPI MyHandler
protectedServer (Authenticated authUser) = jizhangServer authUser
protectedServer _ = throwAll err401

api :: Proxy API
api = Proxy

server :: ServerT API MyHandler
server = authServer :<|> protectedServer :<|> swaggerServer

type AppContext = '[CookieSettings, JWTSettings]

app :: AppEnv -> LogT IO Application
app appEnv = do
  env <- getLoggerEnv
  let jwtCfg = appJWTSettings appEnv
      cookieCfg = defaultCookieSettings
      ctx = cookieCfg :. jwtCfg :. EmptyContext
      nat :: forall x. MyHandler x -> Handler x
      nat =
        runLogT "jizhang" (leLogger env) maxBound
          . logExceptions
          . flip runReaderT appEnv
          . runMyHandler
  middleware <- mkLogMiddleware
  pure $
    middleware $
      const $
        serveWithContext api ctx $
          hoistServerWithContext api (Proxy :: Proxy AppContext) nat server
