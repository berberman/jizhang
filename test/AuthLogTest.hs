{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostic privacy regressions use synthetic canaries through real HTTP and
-- PostgreSQL. Logger queues are drained before searching captured output.
module AuthLogTest (authLogTests) where

import APITest (setupAdmin, withTestAppServerLogger, withTestAppServerLoggerEnv)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (forM_, void)
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Only (..), query_)
import qualified Jizhang.API as API
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types (AdminBootstrap (..))
import Jizhang.API.Types.Auth (LoginResponse (..))
import Jizhang.Config (EnvironmentAuthentication (..), parseJWTSettings)
import qualified Jizhang.OIDC as O
import Jizhang.RequestProtection
  ( RatePolicy (..),
    RequestProtectionConfig (..),
    defaultRequestProtectionConfig,
    newRequestProtection,
  )
import Log (mkLogger, runLogT, showLogMessage, shutdownLogger, waitForLogger)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Cover successful proofs, early protection failures and environment-managed
-- startup while checking that ordinary route diagnostics remain available.
authLogTests :: TestTree
authLogTests =
  testGroup
    "Authentication diagnostic privacy"
    [ testCase "credential SQL and authentication request values are absent while ordinary diagnostics remain" $ do
        messages <- newIORef []
        let collect message = atomicModifyIORef' messages (\old -> (showLogMessage Nothing message : old, ()))
        bracket (mkLogger "auth-capture" collect) shutdownLogger $ \logger -> do
          withTestAppServerLogger logger Nothing id $ \_ port manager _ -> do
            let password = "synthetic-private-password-canary"
                send path value = do
                  request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path)
                  HTTP.httpLbs
                    request
                      { HTTP.method = "POST",
                        HTTP.requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer synthetic-header-canary")],
                        HTTP.requestBody = HTTP.RequestBodyLBS (encode value),
                        HTTP.redirectCount = 0
                      }
                    manager
            registered <-
              send "/auth/register" $
                object
                  [ "registerUsername" .= ("alice" :: T.Text),
                    "registerDisplayName" .= ("Alice" :: T.Text),
                    "registerEmail" .= ("synthetic-private-email@example.test" :: T.Text),
                    "registerPassword" .= (password :: T.Text)
                  ]
            assertEqual "registration succeeded" 200 (statusCode $ HTTP.responseStatus registered)
            loggedIn <- send "/auth/login" $ object ["loginIdentifier" .= ("alice" :: T.Text), "loginPassword" .= password]
            assertEqual "login succeeded" 200 (statusCode $ HTTP.responseStatus loggedIn)
            token <- case eitherDecode (HTTP.responseBody loggedIn) of
              Right session -> pure (accessToken session)
              Left _ -> assertFailure "Login response was not a session" >> pure ""
            forM_
              [ "/auth/oidc/callback?state=synthetic-state-canary&code=synthetic-code-canary&error_description=synthetic-error-canary",
                "/auth/oidc/logout/callback?state=synthetic-logout-canary"
              ]
              $ \path -> do
                request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path)
                void $ HTTP.httpLbs request {HTTP.redirectCount = 0} manager
            ordinary <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/currencies")
            void $ HTTP.httpLbs ordinary manager
            waitForLogger logger
            captured <- T.unlines <$> readIORef messages
            forM_
              [ password,
                "synthetic-private-email@example.test",
                "synthetic-header-canary",
                "synthetic-state-canary",
                "synthetic-code-canary",
                "synthetic-error-canary",
                "synthetic-logout-canary",
                token,
                "$2a$",
                "$2b$",
                "$2y$"
              ]
              $ \secret ->
                assertBool "Authentication value leaked into diagnostics" (not $ secret `T.isInfixOf` captured)
            assertBool "Ordinary route diagnostics are retained" ("/currencies" `T.isInfixOf` captured),
      testCase "protection rejections retain CORS and sensitive headers without logging request secrets" $
        forM_ [413, 429, 503] $ \expectedStatus -> do
          messages <- newIORef []
          clock <- newIORef 0
          let collect message = atomicModifyIORef' messages (\old -> (showLogMessage Nothing message : old, ()))
              generous = RatePolicy 1000000 1000000
              policy =
                defaultRequestProtectionConfig
                  { protectionIpRate = generous,
                    protectionAuthIpRate = RatePolicy 2 2,
                    protectionPrincipalRate = generous,
                    protectionExpensiveRate = generous,
                    protectionCredentialRate = generous,
                    protectionAuthBodyBytes = 128,
                    protectionMaxKeys = if expectedStatus == 503 then 1 else 100
                  }
              allowedOrigin = "https://privacy-client.example.test"
              deniedOrigin = "https://untrusted-client.example.test"
              password = "rejected-password-privacy-canary"
              email = "rejected-email-privacy-canary@example.test"
              header = "rejected-header-privacy-canary"
              query = "rejected-query-privacy-canary"
              body =
                encode $
                  object
                    [ "loginIdentifier" .= (email :: T.Text),
                      "loginPassword" .= (password :: T.Text),
                      "padding" .= T.replicate 128 "x"
                    ]
          -- Only admission time is synthetic; advancing nanoseconds refills quotas
          -- without sleeping or changing authentication/database wall clocks.
          protection <- newRequestProtection policy (readIORef clock)
          bracket (mkLogger "rejection-privacy-capture" collect) shutdownLogger $ \logger ->
            withTestAppServerLogger
              logger
              Nothing
              (\env -> env {appRequestProtection = protection, appClientOrigins = [allowedOrigin]})
              $ \_ port manager _ -> do
                let send path origin = do
                      request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path <> "?state=" <> T.unpack query)
                      HTTP.httpLbs
                        request
                          { HTTP.method = "POST",
                            HTTP.requestHeaders =
                              [ ("Content-Type", "application/json"),
                                ("Authorization", "Bearer " <> TE.encodeUtf8 header),
                                ("X-Privacy-Canary", TE.encodeUtf8 header),
                                ("Origin", TE.encodeUtf8 origin)
                              ],
                            HTTP.requestBody = HTTP.RequestBodyLBS body,
                            HTTP.redirectCount = 0
                          }
                        manager
                if expectedStatus == 429
                  then forM_ [1 :: Int, 2] $ \_ -> do
                    priming <- send "/auth/login" allowedOrigin
                    assertEqual
                      "priming consumes auth credits before body rejection"
                      413
                      (statusCode $ HTTP.responseStatus priming)
                  else pure ()
                forM_ ["/auth/login", "/admin/auth/login", "/admin/auth/session/login"] $ \path ->
                  forM_ [allowedOrigin, deniedOrigin] $ \origin -> do
                    if expectedStatus == 413
                      then atomicModifyIORef' clock (\now -> (now + 60000000000, ()))
                      else pure ()
                    response <- send path origin
                    assertEqual "protection failure status" expectedStatus (statusCode $ HTTP.responseStatus response)
                    let headers = HTTP.responseHeaders response
                        (expectedBody, retryAfter) = case expectedStatus of
                          413 -> ("Request body is too large", Nothing)
                          429 -> ("Too many requests; try again later", Just "30")
                          _ -> ("Server is busy; try again later", Just "1")
                    assertEqual "exact protection response" expectedBody (HTTP.responseBody response)
                    assertEqual "retry delay" retryAfter (lookup "Retry-After" headers)
                    assertEqual "plain text response" (Just "text/plain; charset=utf-8") (lookup "Content-Type" headers)
                    assertEqual "sensitive failure is not cached" (Just "no-store") (lookup "Cache-Control" headers)
                    assertEqual "sensitive failure has no referrer" (Just "no-referrer") (lookup "Referrer-Policy" headers)
                    if path == "/auth/login" && origin == allowedOrigin
                      then do
                        assertEqual
                          "allowed user origin retained"
                          (Just $ TE.encodeUtf8 allowedOrigin)
                          (lookup "Access-Control-Allow-Origin" headers)
                        assertBool "retry header exposed" $
                          maybe False (BS.isInfixOf "Retry-After") (lookup "Access-Control-Expose-Headers" headers)
                      else do
                        assertEqual
                          "no origin allowance for admin or untrusted origin"
                          Nothing
                          (lookup "Access-Control-Allow-Origin" headers)
                        assertEqual
                          "no exposed headers for admin or untrusted origin"
                          Nothing
                          (lookup "Access-Control-Expose-Headers" headers)
                ordinary <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/currencies")
                ordinaryResponse <- HTTP.httpLbs ordinary manager
                assertEqual "bodyless liveness remains available" 200 (statusCode $ HTTP.responseStatus ordinaryResponse)
                waitForLogger logger
                captured <- T.unlines <$> readIORef messages
                forM_ [password, email, header, query] $ \secret ->
                  assertBool "Rejected request value leaked into diagnostics" (not $ secret `T.isInfixOf` captured)
                assertBool
                  "Sensitive rejection diagnostic is captured"
                  (("Authentication request completed " <> T.pack (show expectedStatus)) `T.isInfixOf` captured)
                assertBool "Ordinary route diagnostics are retained" ("/currencies" `T.isInfixOf` captured),
      testCase "environment startup and blocked operator actions never disclose provider, encryption or signing material" $ do
        messages <- newIORef []
        let collect message = atomicModifyIORef' messages (\old -> (showLogMessage Nothing message : old, ()))
            providerSecret = "environment-provider-secret-privacy-canary"
            encryptionKey = TE.decodeUtf8 $ B64.encode $ BS.pack [64 .. 95]
            signingKey = TE.decodeUtf8 $ B64.encode $ BS.pack [96 .. 127]
            desired =
              EnvironmentAuthentication
                False
                True
                False
                "Private deployment provider"
                (Just "https://id.example.test/application/o/private/")
                (Just "private-client")
                (Just providerSecret)
        signing <- either (const $ fail "Synthetic signing key rejected") pure (parseJWTSettings signingKey)
        runtime <-
          O.newAuthRuntime
            (Just "http://127.0.0.1:8080")
            (Just "http://127.0.0.1:3000/")
            (Just encryptionKey)
            ["http://127.0.0.1:3000"]
        bracket (mkLogger "environment-auth-capture" collect) shutdownLogger $ \logger ->
          withTestAppServerLoggerEnv
            logger
            (Just $ AdminBootstrap "operator" "operator-password")
            (\env -> env {appAuthRuntime = runtime, appJWTSettings = signing, appEnvironmentAuthentication = Just desired})
            $ \env clientEnv port manager conn -> do
              token <- setupAdmin clientEnv "operator" "operator-password"
              let send method path value = do
                    initial <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> path)
                    HTTP.httpLbs
                      initial
                        { HTTP.method = method,
                          HTTP.redirectCount = 0,
                          HTTP.requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> TE.encodeUtf8 token)],
                          HTTP.requestBody = HTTP.RequestBodyLBS (encode value)
                        }
                      manager
              [Only ciphertext] <- query_ conn "SELECT client_secret_encrypted FROM auth_settings" :: IO [Only T.Text]
              [Only revision] <- query_ conn "SELECT revision FROM auth_settings" :: IO [Only Integer]
              let draft =
                    object
                      [ "revision" .= revision,
                        "localRegistrationEnabled" .= True,
                        "ssoRegistrationEnabled" .= True,
                        "oidcEnabled" .= False,
                        "providerName" .= ("Attempted edit" :: T.Text),
                        "issuer" .= ("https://id.example.test/" :: T.Text),
                        "clientId" .= ("private-client" :: T.Text),
                        "clientSecret" .= providerSecret
                      ]
              forM_
                [ ("PUT", "/admin/authentication", draft),
                  ("POST", "/admin/authentication/test", draft),
                  ("DELETE", "/admin/authentication/provider?revision=" <> show revision, object [])
                ]
                $ \(method, path, value) -> do
                  response <- send method path value
                  assertEqual "environment-owned mutation blocked" 409 (statusCode $ HTTP.responseStatus response)
              effective <- send "GET" "/admin/authentication" (object [])
              assertEqual "effective settings remain readable" 200 (statusCode $ HTTP.responseStatus effective)
              -- Exercise both successful no-op startup and a safe startup error on
              -- the same rows; no HTTP listener can be returned by the failure.
              void $ runLogT "privacy-restart" logger maxBound (API.app env)
              failed <-
                try
                  ( void $
                      runLogT "privacy-failure" logger maxBound $
                        API.app
                          env {appEnvironmentAuthentication = Just desired {envOidcEnabled = True, envClientSecret = Nothing}}
                  ) ::
                  IO (Either SomeException ())
              errorText <- case failed of
                Right () -> assertFailure "Invalid environment startup returned an application" >> pure ""
                Left err -> pure $ T.pack $ displayException err
              assertBool "startup error is controlled" ("Environment authentication configuration failed: " `T.isInfixOf` errorText)
              waitForLogger logger
              captured <- T.unlines <$> readIORef messages
              let responseText = TE.decodeUtf8 $ LBS.toStrict $ HTTP.responseBody effective
              forM_ [providerSecret, encryptionKey, signingKey, ciphertext, token] $ \secret -> do
                assertBool "Environment secret leaked into diagnostics" (not $ secret `T.isInfixOf` captured)
                assertBool "Environment secret leaked into startup error" (not $ secret `T.isInfixOf` errorText)
                assertBool "Environment secret leaked into settings response" (not $ secret `T.isInfixOf` responseText)
    ]
