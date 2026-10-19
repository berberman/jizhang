{-# LANGUAGE OverloadedStrings #-}

-- | Deployment parsing regressions for stable signing keys, explicit auth ownership
-- and bounded request policies, including errors that must not echo supplied values.
module ConfigTest (configTests) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.UUID as UUID
import Jizhang.API.Types.Admin (AdminBootstrap (..))
import Jizhang.API.Types.User (AuthUser (..))
import Jizhang.Config
import Servant.Auth.Server (makeJWT, verifyJWT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Check parser boundaries and real JWT interoperability without reading process environment.
configTests :: TestTree
configTests =
  testGroup
    "Deployment configuration"
    [ testCase "JWT rejects malformed, noncanonical and wrong-length keys" $ do
        let valid = keyText 32 0
            invalid = ["", "not base64", T.dropEnd 1 valid, valid <> "=", " " <> valid, valid <> "\n", T.replace "/" "_" (keyText 32 255), keyText 31 0, keyText 33 0]
        forM_ invalid $ \value -> assertBool "invalid signing key rejected" (isLeft $ parseJWTSettings value),
      testCase "independent parses preserve signatures while distinct keys reject them" $ do
        first <- parsed $ keyText 32 1
        second <- parsed $ keyText 32 1
        other <- parsed $ keyText 32 2
        let claims = AuthUser UUID.nil (UUID.fromWords 0 0 0 1)
        signed <- makeJWT claims first Nothing
        token <- either (const $ assertFailure "Signing failed" >> pure LBS.empty) pure signed
        accepted <- verifyJWT second (LBS.toStrict token) :: IO (Maybe AuthUser)
        rejected <- verifyJWT other (LBS.toStrict token) :: IO (Maybe AuthUser)
        assertBool "same key verifies user and session" $ case accepted of
          Just user -> authUserId user == authUserId claims && authSessionId user == authSessionId claims
          Nothing -> False
        assertBool "different key rejects signature" $ case rejected of Nothing -> True; Just _ -> False,
      testCase "unset and empty bootstrap pairs disable bootstrap" $
        forM_ [(Nothing, Nothing), (Just "", Nothing), (Nothing, Just ""), (Just "", Just "")] $ \(name, password) ->
          assertBool "bootstrap disabled" $ case parseAdminBootstrap name password of Right Nothing -> True; _ -> False,
      testCase "half-configured bootstrap fails" $
        forM_ [(Just "Alice", Nothing), (Nothing, Just "password"), (Just "Alice", Just ""), (Just "", Just "password")] $ \(name, password) ->
          assertBool "incomplete bootstrap rejected" (isLeft $ parseAdminBootstrap name password),
      testCase "bootstrap preserves exact complete values" $
        case parseAdminBootstrap (Just " Alice λ ") (Just " password λ \n") of
          Right (Just value) -> do
            assertEqual "username" " Alice λ " (adminBootstrapUsername value)
            assertBool "password preserved" (adminBootstrapPassword value == " password λ \n")
          _ -> assertFailure "Complete bootstrap rejected",
      testCase "database ownership rejects attempted environment configuration" $ do
        assertBool "absent source uses database" $ case parseEnvironmentAuthentication M.empty of Right Nothing -> True; _ -> False
        forM_ ["", "Environment", "unknown"] $ \source ->
          assertBool "invalid source rejected" $ isLeft $ parseEnvironmentAuthentication $ M.singleton "AUTH_CONFIG_SOURCE" source
        forM_ ["OIDC_ENABLED", "LOCAL_REGISTRATION_ENABLED", "SSO_REGISTRATION_ENABLED", "OIDC_PROVIDER_NAME", "OIDC_ISSUER", "OIDC_CLIENT_ID", "OIDC_CLIENT_SECRET"] $ \key -> do
          assertBool "unowned nonempty value rejected" $ isLeft $ parseEnvironmentAuthentication $ M.singleton key "canary"
          assertBool "empty placeholder remains database managed" $ case parseEnvironmentAuthentication (M.singleton key "") of Right Nothing -> True; _ -> False,
      testCase "environment ownership requires exact explicit policy booleans" $ do
        let values = M.fromList [("AUTH_CONFIG_SOURCE", "environment"), ("OIDC_ENABLED", "false"), ("LOCAL_REGISTRATION_ENABLED", "false"), ("SSO_REGISTRATION_ENABLED", "true")]
        forM_ ["OIDC_ENABLED", "LOCAL_REGISTRATION_ENABLED", "SSO_REGISTRATION_ENABLED"] $ \key -> do
          assertBool "missing boolean rejected" $ isLeft $ parseEnvironmentAuthentication $ M.delete key values
          forM_ ["", "TRUE", "false ", "secret-canary"] $ \bad ->
            case parseEnvironmentAuthentication (M.insert key bad values) of
              Left message -> assertEqual "safe fixed error" (key <> " must be true or false") message
              Right _ -> assertFailure "Malformed boolean accepted",
      testCase "disabled partial provider preserves exact Unicode and independent secret" $ do
        let values = M.fromList [("AUTH_CONFIG_SOURCE", "environment"), ("OIDC_ENABLED", "false"), ("LOCAL_REGISTRATION_ENABLED", "false"), ("SSO_REGISTRATION_ENABLED", "true"), ("OIDC_CLIENT_SECRET", " λ secret \n"), ("OIDC_PROVIDER_NAME", " Provider λ "), ("OIDC_ISSUER", "https://id.example.test/application/o/ledger/")]
        case parseEnvironmentAuthentication values of
          Right (Just settings) -> do
            assertBool "policies remain explicit" (not (envOidcEnabled settings) && not (envLocalRegistrationEnabled settings) && envSsoRegistrationEnabled settings)
            assertEqual "provider label preserved" " Provider λ " (envProviderName settings)
            assertEqual "issuer path preserved" (Just "https://id.example.test/application/o/ledger/") (envIssuer settings)
            assertEqual "missing client stays null" Nothing (envClientId settings)
            assertBool "provider secret is not an encryption key" (envClientSecret settings == Just " λ secret \n")
          _ -> assertFailure "Disabled partial provider rejected"
        case parseEnvironmentAuthentication (M.delete "OIDC_PROVIDER_NAME" $ M.delete "OIDC_CLIENT_SECRET" values) of
          Right (Just settings) -> do
            assertEqual "default label" "Authentik" (envProviderName settings)
            assertBool "omitted secret means clear" (envClientSecret settings == Nothing)
          _ -> assertFailure "Default provider rejected",
      testCase "request policies reject malformed and out-of-range numbers without echo" $ do
        let limits =
              [ ("RATE_LIMIT_IP_PER_MINUTE", 1000000),
                ("RATE_LIMIT_AUTH_PER_IP_PER_MINUTE", 1000000),
                ("RATE_LIMIT_ACCOUNT_PER_MINUTE", 1000000),
                ("RATE_LIMIT_EXPENSIVE_PER_ACCOUNT_PER_MINUTE", 1000000),
                ("RATE_LIMIT_CREDENTIAL_PER_MINUTE", 1000000),
                ("REQUEST_AUTH_BODY_MAX_BYTES", 1048576),
                ("REQUEST_BODY_MAX_BYTES", 16777216),
                ("REQUEST_BULK_BODY_MAX_BYTES", 67108864),
                ("REQUEST_MAX_IN_FLIGHT", 1024),
                ("REQUEST_MAX_EXPENSIVE_IN_FLIGHT", 64),
                ("RATE_LIMIT_MAX_KEYS", 1000000)
              ]
        forM_ limits $ \(key, upper) ->
          forM_ (["", "0", "-1", "+1", " 1", "1 ", "1.0", "1e2", "１２", "secret-canary", T.replicate 100 "9", T.pack $ show (upper + 1 :: Integer)]) $ \value ->
            case parseRequestProtectionConfig (M.singleton key value) of
              Left message -> message @?= key <> " is outside its permitted range"
              Right _ -> assertFailure "invalid numeric policy accepted",
      testCase "proxy policy rejects malformed CIDRs and cross-field limits" $ do
        forM_
          [ "192.0.2.1",
            "example.test/24",
            "192.0.2.1/33",
            "2001:db8::/129",
            "192.0.2.1/-1",
            "192.0.2.1/24junk",
            "192.0.2.1/24,",
            ",192.0.2.1/24",
            "[2001:db8::]/32",
            "fe80::1%lo0/64",
            T.intercalate "," (replicate 33 "192.0.2.0/24")
          ]
          $ \value ->
            case parseRequestProtectionConfig (M.singleton "TRUSTED_PROXY_CIDRS" value) of
              Left message -> message @?= "TRUSTED_PROXY_CIDRS must contain at most 32 valid IPv4/IPv6 CIDRs"
              Right _ -> assertFailure "invalid proxy policy accepted"
        forM_
          [ [("REQUEST_AUTH_BODY_MAX_BYTES", "100"), ("REQUEST_BODY_MAX_BYTES", "99")],
            [("REQUEST_BODY_MAX_BYTES", "9000000")],
            [("REQUEST_MAX_IN_FLIGHT", "1")]
          ]
          $ \values ->
            assertBool "cross-field limits rejected" $ isLeft $ parseRequestProtectionConfig $ M.fromList values
    ]
  where
    parsed value = either (const $ assertFailure "Valid key rejected" >> fail "Invalid fixture") pure $ parseJWTSettings value

-- | Build canonical Base64 keys from repeated bytes; size selects valid or boundary fixtures.
keyText :: Int -> Word -> Text
keyText size byte = decodeUtf8 $ B64.encode $ BS.replicate size (fromIntegral byte)
