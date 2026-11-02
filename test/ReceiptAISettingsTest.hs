{-# LANGUAGE OverloadedStrings #-}

module ReceiptAISettingsTest (receiptAISettingsTests) where

import APITest (identityRequest, setupAdmin, setupUser, withTestAppServer, withTestAppServerLogger)
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Jizhang.API.App (AppEnv (..))
import Jizhang.API.Types (AdminBootstrap (..))
import Jizhang.API.Types.ReceiptAISettings
import Jizhang.Auth.Security (decryptSecret, encryptSecret)
import Jizhang.ReceiptAI.Runtime
import Jizhang.RequestProtection (RequestProtectionConfig (..), requestProtectionConfig)
import Log (mkLogger, showLogMessage, shutdownLogger, waitForLogger)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status
import Test.Tasty
import Test.Tasty.HUnit

path :: String
path = "/admin/receipt-ai/settings"

bootstrap :: Maybe AdminBootstrap
bootstrap = Just $ AdminBootstrap "admin" "adminpass123"

secret, encryption :: Text
secret = "synthetic-receipt-settings-secret-canary"
encryption = TE.decodeUtf8 $ B64.encode $ BS.pack [0 .. 31]

configuration :: Maybe Text -> Maybe Text -> ReceiptAIConfig
configuration environment key = either (error . T.unpack) id $ parseReceiptAIConfig environment key

requestBody :: Int64 -> Bool -> Text -> Maybe Text -> Value
requestBody revision enabled action key = toJSON $ ReceiptAISettingsRequest revision enabled action key

readSettings :: HTTP.Response LBS.ByteString -> IO ReceiptAISettings
readSettings response = do
  HTTP.responseStatus response @?= status200
  either (\message -> assertFailure message >> fail message) pure $ eitherDecode $ HTTP.responseBody response

receiptAISettingsTests :: TestTree
receiptAISettingsTests = testGroup "Receipt AI settings"
  [ testCase "revisioned replacement, keep and clear preserve write-only encrypted credentials" $
      withTestAppServer bootstrap (\env -> env {appReceiptAIConfig = configuration Nothing (Just encryption)}) $ \env port manager conn -> do
        token <- setupAdmin env "admin" "adminpass123"
        let get = identityRequest port manager (Just token) "GET" path Null >>= readSettings
            put revision enabled action key = identityRequest port manager (Just token) "PUT" path (requestBody revision enabled action key)
        initial <- get
        rasunavailableReason initial @?= Just "disabled"
        saved <- put 0 True "replace" (Just secret) >>= readSettings
        rasrevision saved @?= 1
        rasavailable saved @?= True
        [Only ciphertext] <- query_ conn "SELECT api_key_encrypted FROM receipt_ai_settings" :: IO [Only Text]
        assertBool "encrypted at rest" (ciphertext /= secret)
        key <- maybe (assertFailure "Missing synthetic encryption key" >> fail "Missing key") pure $ receiptAIEncryptionKey $ configuration Nothing (Just encryption)
        decryptSecret key receiptAIKeyAad ciphertext @?= Right secret
        kept <- put 1 False "keep" Nothing >>= readSettings
        rasrevision kept @?= 2
        query_ conn "SELECT api_key_encrypted FROM receipt_ai_settings" >>= (@?= [Only ciphertext])
        put 1 True "clear" Nothing >>= \response -> HTTP.responseStatus response @?= status409
        afterStale <- get
        afterStale @?= kept
        cleared <- put 2 True "clear" Nothing >>= readSettings
        raskeyConfigured cleared @?= False
        rasunavailableReason cleared @?= Just "key_missing"
        query_ conn "SELECT api_key_encrypted FROM receipt_ai_settings" >>= (@?= [Only (Nothing :: Maybe Text)])
        forM_ [secret, ciphertext] $ \value -> assertBool "response exposes no key material" (not $ TE.encodeUtf8 value `BS.isInfixOf` LBS.toStrict (encode saved)),
    testCase "environment override rejects key edits and keeps the retained database key" $
      withTestAppServer bootstrap (\env -> env {appReceiptAIConfig = configuration (Just secret) Nothing}) $ \env port manager conn -> do
        let retainedConfig = configuration Nothing (Just encryption)
        encryptionKey <- maybe (assertFailure "Missing synthetic encryption key" >> fail "Missing key") pure $ receiptAIEncryptionKey retainedConfig
        ciphertext <- encryptSecret encryptionKey receiptAIKeyAad "retained-database-credential"
        void $ execute conn "UPDATE receipt_ai_settings SET api_key_encrypted=?" (Only ciphertext)
        token <- setupAdmin env "admin" "adminpass123"
        let put action key = identityRequest port manager (Just token) "PUT" path (requestBody 0 True action key)
        forM_ [("replace", Just "new-synthetic-key"), ("clear", Nothing)] $ \(action, key) ->
          put action key >>= \response -> HTTP.responseStatus response @?= status409
        saved <- put "keep" Nothing >>= readSettings
        rasavailable saved @?= True
        raskeySource saved @?= "environment"
        rascanStoreKey saved @?= False
        query_ conn "SELECT api_key_encrypted FROM receipt_ai_settings" >>= (@?= [Only ciphertext])
        case effectiveReceiptAIKey retainedConfig (Just ciphertext) of
          Left _ -> assertFailure "Retained database credential no longer usable without override"
          Right key -> receiptAIKeyText key @?= "retained-database-credential",
    testCase "missing encryption blocks replacement without breaking settings and manual mode" $
      withTestAppServer bootstrap id $ \env port manager _ -> do
        token <- setupAdmin env "admin" "adminpass123"
        response <- identityRequest port manager (Just token) "PUT" path (requestBody 0 True "replace" (Just secret))
        HTTP.responseStatus response @?= status503
        saved <- identityRequest port manager (Just token) "PUT" path (requestBody 0 True "keep" Nothing) >>= readSettings
        rasunavailableReason saved @?= Just "key_missing"
        rascanStoreKey saved @?= False,
    testCase "unreadable ciphertext and wrong-purpose encryption cannot become available" $
      forM_ [Nothing, Just encryption] $ \encryptionKey ->
        withTestAppServer bootstrap (\env -> env {appReceiptAIConfig = configuration Nothing encryptionKey}) $ \env port manager conn -> do
          key <- maybe (assertFailure "Missing synthetic encryption key" >> fail "Missing key") pure $ receiptAIEncryptionKey $ configuration Nothing (Just encryption)
          ciphertext <- encryptSecret key "wrong-purpose" secret
          void $ execute conn "UPDATE receipt_ai_settings SET enabled=true,api_key_encrypted=?" (Only ciphertext)
          token <- setupAdmin env "admin" "adminpass123"
          settings <- identityRequest port manager (Just token) "GET" path Null >>= readSettings
          rasunavailableReason settings @?= Just "stored_key_unreadable"
          raskeyConfigured settings @?= True
          void $ execute_ conn "UPDATE receipt_ai_settings SET enabled=false"
          disabled <- identityRequest port manager (Just token) "GET" path Null >>= readSettings
          rasunavailableReason disabled @?= Just "disabled",
    testCase "upload admission limit takes precedence over disabled or missing credentials" $
      withTestAppServer bootstrap (\env -> env {appRequestProtection = (appRequestProtection env) {requestProtectionConfig = (requestProtectionConfig $ appRequestProtection env) {protectionBulkBodyBytes = 1}}}) $ \env port manager _ -> do
        token <- setupAdmin env "admin" "adminpass123"
        settings <- identityRequest port manager (Just token) "GET" path Null >>= readSettings
        rasunavailableReason settings @?= Just "upload_limit",
    testCase "normal accounts cannot read or mutate settings and request keys are exact" $
      withTestAppServer bootstrap id $ \env port manager _ -> do
        user <- setupUser env "alice"
        forM_ [Nothing, Just user] $ \token -> forM_ ["GET", "PUT"] $ \method -> do
          response <- identityRequest port manager token method path (requestBody 0 True "keep" Nothing)
          HTTP.responseStatus response @?= status401
        admin <- setupAdmin env "admin" "adminpass123"
        forM_ [object ["revision" .= (0 :: Int), "enabled" .= True, "keyAction" .= ("keep" :: Text)],
               object ["revision" .= (0 :: Int), "enabled" .= True, "keyAction" .= ("keep" :: Text), "apiKey" .= secret],
               object ["revision" .= (0 :: Int), "enabled" .= True, "keyAction" .= ("replace" :: Text), "apiKey" .= ("bad key" :: Text)],
               object ["revision" .= (0 :: Int), "enabled" .= True, "keyAction" .= ("clear" :: Text), "apiKey" .= Null, "extra" .= True]] $ \body ->
          identityRequest port manager (Just admin) "PUT" path body >>= \response -> HTTP.responseStatus response @?= status400,
    testCase "maximum revision is rejected before arithmetic or persistence" $
      withTestAppServer bootstrap id $ \env port manager conn -> do
        void $ execute conn "UPDATE receipt_ai_settings SET revision=?" (Only (maxBound :: Int64))
        token <- setupAdmin env "admin" "adminpass123"
        response <- identityRequest port manager (Just token) "PUT" path (requestBody maxBound True "keep" Nothing)
        HTTP.responseStatus response @?= status409
        query_ conn "SELECT revision,enabled FROM receipt_ai_settings" >>= (@?= [(maxBound :: Int64, False)]),
    testCase "admin cookie mutations require matching CSRF" $
      withTestAppServer bootstrap id $ \_ port manager conn -> do
        login <- HTTP.parseRequest $ "http://127.0.0.1:" <> show port <> "/admin/auth/session/login"
        response <- HTTP.httpLbs (login {HTTP.method = "POST", HTTP.requestHeaders = [("Content-Type", "application/json")], HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ object ["username" .= ("admin" :: Text), "password" .= ("adminpass123" :: Text)]}) manager
        HTTP.responseStatus response @?= status200
        let cookies = HTTP.destroyCookieJar $ HTTP.responseCookieJar response
            xsrf = [HTTP.cookie_value cookie | cookie <- cookies, HTTP.cookie_name cookie == "XSRF-TOKEN"]
        csrf <- case xsrf of [value] -> pure value; _ -> assertFailure "Missing CSRF cookie" >> fail "Missing CSRF cookie"
        initial <- HTTP.parseRequest $ "http://127.0.0.1:" <> show port <> path
        let put headers = HTTP.httpLbs (initial {HTTP.method = "PUT", HTTP.cookieJar = Just $ HTTP.responseCookieJar response, HTTP.requestHeaders = [("Content-Type", "application/json")] <> headers, HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ requestBody 0 True "keep" Nothing}) manager
        forM_ [[], [("X-XSRF-TOKEN", "wrong")]] $ \headers -> put headers >>= \denied -> HTTP.responseStatus denied @?= status401
        query_ conn "SELECT revision FROM receipt_ai_settings" >>= (@?= [Only (0 :: Int64)])
        put [("X-XSRF-TOKEN", csrf)] >>= readSettings >>= \saved -> rasrevision saved @?= 1,
    testCase "Trace and response bodies never disclose replacement key or ciphertext" $ do
      messages <- newIORef []
      let collect message = atomicModifyIORef' messages (\values -> (showLogMessage Nothing message : values, ()))
      bracket (mkLogger "receipt-settings-privacy" collect) shutdownLogger $ \logger ->
        withTestAppServerLogger logger bootstrap (\env -> env {appReceiptAIConfig = configuration Nothing (Just encryption)}) $ \env port manager conn -> do
          token <- setupAdmin env "admin" "adminpass123"
          identityRequest port manager (Just token) "PUT" path (requestBody 0 True "replace" $ Just secret) >>= readSettings >>= \settings -> rasavailable settings @?= True
          response <- identityRequest port manager (Just token) "GET" path Null
          [Only ciphertext] <- query_ conn "SELECT api_key_encrypted FROM receipt_ai_settings" :: IO [Only Text]
          waitForLogger logger
          captured <- T.unlines <$> readIORef messages
          forM_ [secret, ciphertext, token] $ \value -> do
            assertBool "secret in Trace" (not $ value `T.isInfixOf` captured)
            assertBool "secret in GET" (not $ TE.encodeUtf8 value `BS.isInfixOf` LBS.toStrict (HTTP.responseBody response))
  ]
