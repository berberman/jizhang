{-# LANGUAGE OverloadedStrings #-}

-- | Pure JSON contract regressions for strict request keys, explicit nullability
-- and safe response shapes. No database, provider or wall clock is required.
module OIDCDtoTest (oidcDtoTests) where

import Data.Aeson (FromJSON, Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.List (sort)
import Data.Proxy (Proxy (..))
import Data.Time (UTCTime (..), fromGregorian)
import qualified Data.UUID as UUID
import qualified Jizhang.API.Types.Auth as Auth
import qualified Jizhang.API.Types.OIDC as O
import qualified Jizhang.API.Types.User as User
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

-- Never render a failed decode: a successful value can contain credentials.
decodeAs :: (FromJSON a) => Proxy a -> Value -> Either String a
decodeAs _ = eitherDecode . encode

-- | Assert decode success without rendering credential-bearing decoded values.
accepts :: (FromJSON a) => Proxy a -> Value -> Assertion
accepts proxy value = assertBool "Contract value must decode" (not (isLeft (decodeAs proxy value)))

rejects :: (FromJSON a) => Proxy a -> Value -> Assertion
rejects proxy value = assertBool "Invalid contract value must be rejected" (isLeft (decodeAs proxy value))

without :: Key -> Value -> Value
without key (Object fields) = Object (KM.delete key fields)
without _ value = value

with :: Key -> Value -> Value -> Value
with key value (Object fields) = Object (KM.insert key value fields)
with _ _ value = value

-- | Compare the complete key set, catching accidental response-field disclosure.
assertKeys :: [Key] -> Value -> Assertion
assertKeys expected (Object fields) = sort (KM.keys fields) @?= sort expected
assertKeys _ _ = assertFailure "Expected an encoded object"

assertFieldKeys :: Key -> [Key] -> Value -> Assertion
assertFieldKeys key expected (Object fields) = case KM.lookup key fields of
  Just value -> assertKeys expected value
  Nothing -> assertFailure "Expected nested response field"
assertFieldKeys _ _ _ = assertFailure "Expected an encoded object"

-- | Stable timestamps keep serialization fixtures independent of the execution date.
time :: UTCTime
time = UTCTime (fromGregorian 2026 10 17) 0

user :: User.User
user = User.User (User.UserId (UUID.fromWords 0 0 0 1)) "synthetic" "Synthetic User"

profile :: User.UserProfile
profile = User.UserProfile user "synthetic@example.test" False

identity :: O.IdentitySummary
identity =
  O.IdentitySummary
    "https://id.example.test/application/o/app/"
    "synthetic-subject"
    "Authentik"
    Nothing
    False
    time
    Nothing

settingsRequest :: O.AuthenticationSettingsRequest
settingsRequest = O.AuthenticationSettingsRequest 7 True True False "Authentik" Nothing Nothing Nothing

settings :: O.AuthenticationSettings
settings =
  O.AuthenticationSettings
    7
    False
    True
    True
    False
    "Authentik"
    Nothing
    Nothing
    True
    Nothing
    Nothing
    Nothing
    False
    (Just "Runtime configuration is unavailable")
    0

oidcDtoTests :: TestTree
oidcDtoTests =
  testGroup
    "OIDC DTO security contracts"
    [ testCase "nullable request keys require explicit null, not omission" $ do
        let start = toJSON (O.OidcStartRequest "synthetic-challenge" Nothing)
            password = toJSON (O.PasswordChangeRequest Nothing "synthetic-password")
            config = toJSON settingsRequest
        accepts (Proxy :: Proxy O.OidcStartRequest) start
        rejects (Proxy :: Proxy O.OidcStartRequest) (without "invitationToken" start)
        accepts (Proxy :: Proxy O.PasswordChangeRequest) password
        rejects (Proxy :: Proxy O.PasswordChangeRequest) (without "currentPassword" password)
        accepts (Proxy :: Proxy O.AuthenticationSettingsRequest) config
        rejects (Proxy :: Proxy O.AuthenticationSettingsRequest) (without "clientSecret" config),
      testCase "requests reject extra fields, including caller-selected link targets" $ do
        rejects (Proxy :: Proxy O.OidcStartRequest) $
          with "redirectUrl" (String "https://attacker.example/") (toJSON (O.OidcStartRequest "challenge" Nothing))
        rejects (Proxy :: Proxy O.OidcContinuationRequest) $
          object ["continuationToken" .= ("synthetic-continuation" :: String), "userId" .= UUID.nil]
        rejects (Proxy :: Proxy O.AuthenticationSettingsRequest) $
          with "hasClientSecret" (Bool True) (toJSON settingsRequest),
      testCase "authentication ownership is a required response-only boolean" $ do
        let proxy = Proxy :: Proxy O.AuthenticationSettings
            value = toJSON settings
        accepts proxy value
        accepts proxy (with "environmentManaged" (Bool True) value)
        rejects proxy (without "environmentManaged" value)
        mapM_
          (\invalid -> rejects proxy (with "environmentManaged" invalid value))
          [Null, String "false", Number 0]
        mapM_
          ( \managed ->
              rejects
                (Proxy :: Proxy O.AuthenticationSettingsRequest)
                (with "environmentManaged" (Bool managed) (toJSON settingsRequest))
          )
          [False, True],
      testCase "enums reject constructor names, unknown values and wrong JSON types" $ do
        accepts (Proxy :: Proxy O.AccountOidcPurpose) (String "link")
        accepts (Proxy :: Proxy O.SessionMethod) (String "password")
        accepts (Proxy :: Proxy O.InvitationPurpose) (String "password_reset")
        accepts (Proxy :: Proxy O.OidcError) (String "access_denied")
        accepts (Proxy :: Proxy O.AdminCreateMode) (String "sso")
        mapM_ (rejects (Proxy :: Proxy O.AccountOidcPurpose)) [String "LinkIdentity", String "login", Null]
        mapM_ (rejects (Proxy :: Proxy O.SessionMethod)) [String "PasswordMethod", String "sso", Bool True]
        rejects (Proxy :: Proxy O.InvitationPurpose) (String "password-reset")
        rejects (Proxy :: Proxy O.OidcError) (String "raw_provider_error")
        rejects (Proxy :: Proxy O.AdminCreateMode) (String "SSO")
        rejects (Proxy :: Proxy O.AccountOidcStartRequest) $
          object ["purpose" .= ("login" :: String), "browserChallenge" .= ("challenge" :: String)],
      testCase "exchange variants reject cross-branch fields and missing decisions" $ do
        let authenticated = toJSON (O.OidcAuthenticated (Auth.LoginResponse "synthetic-token" user))
            registration =
              toJSON
                ( O.OidcRegistrationRequired
                    "synthetic-continuation"
                    (O.OidcProfileHints Nothing Nothing Nothing False)
                    True
                )
            link =
              toJSON
                ( O.OidcLinkConfirmationRequired
                    "synthetic-continuation"
                    user
                    (O.IdentityCandidate "https://id.example.test/" "synthetic-subject" "Authentik" Nothing False)
                )
            proxy = Proxy :: Proxy O.OidcExchangeResponse
        mapM_ (accepts proxy) [authenticated, registration, link]
        rejects proxy (with "continuationToken" (String "synthetic-continuation") authenticated)
        rejects proxy (with "session" (toJSON (Auth.LoginResponse "synthetic-token" user)) registration)
        rejects proxy (with "canRegister" (Bool True) link)
        rejects proxy (without "session" authenticated)
        rejects proxy (without "canRegister" registration)
        rejects proxy (without "target" link)
        rejects proxy (without "status" authenticated)
        rejects proxy (with "status" (String "Authenticated") authenticated),
      testCase "identity summaries retain required nullable claim and login keys" $ do
        let value = toJSON identity
            proxy = Proxy :: Proxy O.IdentitySummary
        accepts proxy value
        rejects proxy (without "claimedEmail" value)
        rejects proxy (without "lastLoginAt" value)
        rejects proxy (with "linkedAt" Null value),
      testCase "admin encodings expose only safe settings, profiles and linked identity metadata" $ do
        let summary = toJSON (O.AdminUserSummary profile True False True)
            details =
              toJSON
                ( O.AdminUserDetails
                    profile
                    True
                    False
                    (Just identity)
                    [O.PendingInvitation UUID.nil O.OidcLinkInvitation time]
                )
        assertKeys
          [ "revision",
            "environmentManaged",
            "localRegistrationEnabled",
            "ssoRegistrationEnabled",
            "oidcEnabled",
            "providerName",
            "issuer",
            "clientId",
            "hasClientSecret",
            "callbackUrl",
            "backchannelLogoutUrl",
            "postLogoutRedirectUrl",
            "runtimeReady",
            "runtimeError",
            "ssoOnlyUserCount"
          ]
          (toJSON settings)
        assertKeys ["profile", "enabled", "hasPassword", "hasIdentity"] summary
        assertKeys ["profile", "enabled", "hasPassword", "identity", "invitations"] details
        assertFieldKeys "profile" ["user", "email", "emailVerified"] details
        assertFieldKeys
          "identity"
          ["issuer", "subject", "providerName", "claimedEmail", "claimedEmailVerified", "linkedAt", "lastLoginAt"]
          details
        case details of
          Object fields -> case KM.lookup "invitations" fields of
            Just (Array invitations) -> mapM_ (assertKeys ["invitationId", "purpose", "expiresAt"]) invitations
            _ -> assertFailure "Expected pending invitation metadata"
          _ -> assertFailure "Expected user details"
    ]
