{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Compose public, bearer-user and operator routes into the WAI application.
-- Middleware here owns admission, CORS and sensitive-route logging/header policy.
module Jizhang.API where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), asks)
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import Data.Swagger (Swagger)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (getCurrentTime)
import Jizhang.API.Admin
import Jizhang.API.Admin.Authentication (applyEnvironmentAuthentication)
import Jizhang.API.AdminAuth (AdminAuthAPI, adminAuthServer, adminCookieSettings, bootstrapAdmin)
import Jizhang.API.AdminUI
import Jizhang.API.Auth
import Jizhang.API.Common ()
import Jizhang.API.Group
import Jizhang.API.OIDC (OIDCAPI, oidcServer)
import Jizhang.API.Receipt
import Jizhang.API.Record
import Jizhang.API.Report
import Jizhang.API.RequestProtection (enforcePrincipalLimit)
import Jizhang.API.App
import Jizhang.API.Types (AuthAdmin (authAdminId), AuthUser (authUserId), CurrencyInfo)
import Jizhang.API.User
import Jizhang.Auth.Session (requireActiveUserSession)
import Jizhang.RequestProtection (Principal (..), requestProtectionMiddleware)
import Log (LogLevel (LogAttention, LogInfo), LogT, LoggerEnv (leLogger, leMaxLogLevel), MonadLog (getLoggerEnv), getLoggerIO, logExceptions, runLogT)
import Network.HTTP.Types.Status (status400, statusCode)
import Network.Wai (mapResponseHeaders, pathInfo, queryString, rawPathInfo, requestMethod, responseLBS, responseStatus)
import qualified Network.Wai.Middleware.Cors as Cors
import Servant hiding (respond)
import Servant.Auth.Server
import Servant.Auth.Swagger ()
import Servant.Swagger
import Web.Cookie (parseSetCookie, setCookieMaxAge)

-- | Assemble user feature handlers; 'protectedServer' supplies the session and rate gates.
jizhangServer :: AuthUser -> MyServer JizhangAPI
jizhangServer authUser =
  userServer authUser
    :<|> groupServer authUser
    :<|> recordServer authUser
    :<|> reportServer authUser
    :<|> receiptServer authUser

-- | Generated JSON contract for API clients; the static operator UI is excluded.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Public metadata for currency input and exact minor-unit display.
type CurrencyAPI = "currencies" :> Get '[JSON] [CurrencyInfo]

currencyServer :: MyServer CurrencyAPI
currencyServer = M.elems <$> asks appCurrencies

-- | User-account and ledger routes sharing bearer authentication.
type JizhangAPI = UserAPI :<|> GroupAPI :<|> RecordAPI :<|> ReportAPI :<|> ReceiptAPI

-- | JWT decoding alone is insufficient: 'protectedServer' checks the stored user session.
type ProtectedAPI = Auth '[JWT] AuthUser :> JizhangAPI

-- | Separate operator identity accepted via JWT or same-origin cookie authentication.
type ProtectedAdminAPI = Auth '[JWT, Cookie] AuthAdmin :> AdminAPI

-- | Full HTTP surface, including unauthenticated discovery, authentication and static UI.
type API = AuthAPI :<|> OIDCAPI :<|> AdminAuthAPI :<|> AdminUIAPI :<|> ProtectedAPI :<|> ProtectedAdminAPI :<|> SwaggerAPI :<|> CurrencyAPI

swaggerServer :: MyServer SwaggerAPI
swaggerServer = pure $ toSwagger (Proxy :: Proxy (AuthAPI :<|> OIDCAPI :<|> AdminAuthAPI :<|> ProtectedAPI :<|> ProtectedAdminAPI :<|> CurrencyAPI))

-- | Reject revoked/expired user sessions and enforce the account request budget before handlers.
protectedServer :: AuthResult AuthUser -> ServerT JizhangAPI MyHandler
protectedServer (Authenticated authUser) =
  hoistServer
    (Proxy :: Proxy JizhangAPI)
    (\action -> requireActiveUserSession authUser >> enforcePrincipalLimit (UserPrincipal $ authUserId authUser) >> action)
    (jizhangServer authUser)
protectedServer _ = throwAll err401

-- | Enforce the operator request budget after framework authentication succeeds.
protectedAdminServer :: AuthResult AuthAdmin -> ServerT AdminAPI MyHandler
protectedAdminServer (Authenticated authAdmin) =
  hoistServer (Proxy :: Proxy AdminAPI)
    (\action -> enforcePrincipalLimit (AdminPrincipal $ authAdminId authAdmin) >> action)
    (adminServer authAdmin)
protectedAdminServer _ = throwAll err401

api :: Proxy API
api = Proxy

server :: ServerT API MyHandler
server = authServer :<|> oidcServer :<|> adminAuthServer :<|> adminUIServer :<|> protectedServer :<|> protectedAdminServer :<|> swaggerServer :<|> currencyServer

-- | Framework authentication settings shared by the normal and sensitive dispatch paths.
type AppContext = '[CookieSettings, JWTSettings]

-- | Apply configured authentication ownership and optional operator bootstrap, then serve routes.
-- Authentication-sensitive requests omit query/body logging and receive no-store responses.
app :: AppEnv -> LogT IO Application
app appEnv = do
  env <- getLoggerEnv
  let jwtCfg = appJWTSettings appEnv
      cookieCfg = adminCookieSettings (appAdminCookieSecure appEnv)
      ctx = cookieCfg :. jwtCfg :. EmptyContext
      origins = map encodeUtf8 (appClientOrigins appEnv)
      corsPolicy request = case pathInfo request of
        segment : _
          | segment `elem` ["auth", "users", "groups", "currencies"],
            not (null origins) ->
              Just $
                Cors.simpleCorsResourcePolicy
                  { Cors.corsOrigins = Just (origins, False),
                    Cors.corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
                    Cors.corsRequestHeaders = ["Authorization", "Content-Type"],
                    Cors.corsExposedHeaders = Just ["Retry-After"],
                    Cors.corsRequireOrigin = False,
                    Cors.corsVaryOrigin = True,
                    Cors.corsIgnoreFailures = True
                  }
        _ -> Nothing
      nat :: forall x. MyHandler x -> Handler x
      nat =
        runLogT "jizhang" (leLogger env) (leMaxLogLevel env)
          . logExceptions
          . flip runReaderT appEnv
          . runMyHandler
      application = serveWithContext api ctx $ hoistServerWithContext api (Proxy :: Proxy AppContext) nat server
      -- Sensitive exceptions bypass logExceptions to avoid logging provider credentials.
      sensitiveNat :: forall x. MyHandler x -> Handler x
      sensitiveNat = runLogT "jizhang" (leLogger env) (leMaxLogLevel env) . flip runReaderT appEnv . runMyHandler
      sensitiveApplication = serveWithContext api ctx $ hoistServerWithContext api (Proxy :: Proxy AppContext) sensitiveNat server
      dispatch request respond
        | pathInfo request `elem` [["admin-ui"], ["admin-ui", ""]] =
            application request $ respond . mapResponseHeaders
              (\headers -> ("Content-Security-Policy", "frame-ancestors 'none'") : ("X-Frame-Options", "DENY") : headers)
        | sensitiveRoute (pathInfo request) =
            if validCallbackQuery request then sensitiveApplication request respond
            else respond $ responseLBS status400 [] "Invalid OIDC callback"
        | otherwise = application request respond
      operatorSessionPolicy request respond
        | "admin" : _ <- pathInfo request,
          pathInfo request /= ["admin", "auth", "session", "login"] =
            -- Framework Auth refreshes cookies without preserving the login JWT expiry.
            dispatch request $ respond . mapResponseHeaders
              (filter (\(name, value) -> name /= "Set-Cookie" || setCookieMaxAge (parseSetCookie value) == Just 0))
        | otherwise = dispatch request respond
      admittedApplication = Cors.cors corsPolicy $ requestProtectionMiddleware (appRequestProtection appEnv) operatorSessionPolicy
  forM_ (appEnvironmentAuthentication appEnv) $ \configuration -> do
    result <- liftIO $ runHandler $ sensitiveNat $ applyEnvironmentAuthentication configuration
    case result of
      Left failure -> liftIO $ fail $ "Environment authentication configuration failed: " <> BL.unpack (errBody failure)
      Right () -> pure ()
  bootstrapAdmin appEnv
  logger <- getLoggerIO
  pure $
    ( \request respond ->
        if sensitiveRoute (pathInfo request)
          then admittedApplication request
            $ \response -> do
              now <- getCurrentTime
              let code = statusCode $ responseStatus response
                  level = if code >= 500 then LogAttention else LogInfo
              logger now level ("Authentication request completed " <> T.pack (show code)) emptyObject
              respond $
                mapResponseHeaders
                  ( \headers ->
                      ("Cache-Control", "no-store")
                        : ("Referrer-Policy", "no-referrer")
                        : filter (\(name, _) -> name /= "Cache-Control" && name /= "Referrer-Policy") headers
                  )
                  response
          else admittedApplication request $ \response -> do
            now <- getCurrentTime
            let code = statusCode $ responseStatus response
                level = if code >= 500 then LogAttention else LogInfo
                message = "HTTP request completed " <> decodeUtf8With lenientDecode (requestMethod request)
                  <> " " <> decodeUtf8With lenientDecode (rawPathInfo request) <> " " <> T.pack (show code)
            logger now level message emptyObject
            respond response
    )
  where
    -- QueryParams discards valueless entries; count raw occurrences before routing.
    validCallbackQuery request =
      let values key = [value | (name, value) <- queryString request, name == key]
          nonemptySingle [Just value] = value /= ""
          nonemptySingle _ = False
          optionalSingle entries = null entries || nonemptySingle entries
       in case pathInfo request of
            ["auth", "oidc", "callback"] ->
              nonemptySingle (values "state")
                && optionalSingle (values "code")
                && optionalSingle (values "error")
                && optionalSingle (values "iss")
                && (nonemptySingle (values "code") /= nonemptySingle (values "error"))
            ["auth", "oidc", "logout"] -> nonemptySingle (values "token")
            ["auth", "oidc", "logout", "callback"] ->
              nonemptySingle (values "state") && optionalSingle (values "error")
            _ -> True
    sensitiveRoute ("auth" : _) = True
    sensitiveRoute ("admin" : "auth" : _) = True
    sensitiveRoute ("admin" : "authentication" : _) = True
    sensitiveRoute ("admin" : "users" : _) = True
    sensitiveRoute ("admin" : "admins" : _) = True
    sensitiveRoute ("users" : "me" : operation : _) = operation `elem` ["authentication", "reauthenticate", "oidc", "password", "identity", "logout"]
    sensitiveRoute _ = False
