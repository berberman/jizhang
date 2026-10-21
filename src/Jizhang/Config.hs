{-# LANGUAGE OverloadedStrings #-}

-- | Startup parsers for signing keys, bootstrap credentials and request limits.
-- Authentication options have one owner: database defaults or explicit environment mode.
module Jizhang.Config
  ( parseJWTSettings,
    parseAdminBootstrap,
    EnvironmentAuthentication (..),
    parseEnvironmentAuthentication,
    parseRequestProtectionConfig,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IP (IP (..), IPRange (..), makeAddrRange)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Jizhang.API.Types.Admin (AdminBootstrap (..))
import Jizhang.RequestProtection
import Text.Read (readMaybe)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings, fromSecret)

-- | Accept only a canonical 32-byte symmetric JWT signing key, not a passphrase.
parseJWTSettings :: Text -> Either Text JWTSettings
parseJWTSettings value = case B64.decode (encodeUtf8 value) of
  Right bytes
    | BS.length bytes == 32,
      decodeUtf8 (B64.encode bytes) == value ->
        Right $ defaultJWTSettings $ fromSecret bytes
  _ -> Left "JWT_SECRET_KEY must be canonical standard base64 encoding of exactly 32 bytes"

-- | Empty values count as absent; a partial credential pair is a startup error.
parseAdminBootstrap :: Maybe Text -> Maybe Text -> Either Text (Maybe AdminBootstrap)
parseAdminBootstrap username password = case (nonempty username, nonempty password) of
  (Nothing, Nothing) -> Right Nothing
  (Just name, Just secret) -> Right $ Just $ AdminBootstrap name secret
  _ -> Left "Set both ADMIN_USERNAME and ADMIN_PASSWORD, or neither"
  where
    nonempty = (>>= \value -> if T.null value then Nothing else Just value)

-- Deliberately no Show or JSON instances: this contains a provider credential.
data EnvironmentAuthentication = EnvironmentAuthentication
  { envLocalRegistrationEnabled :: !Bool,
    envSsoRegistrationEnabled :: !Bool,
    envOidcEnabled :: !Bool,
    envProviderName :: !Text,
    envIssuer :: !(Maybe Text),
    envClientId :: !(Maybe Text),
    envClientSecret :: !(Maybe Text)
  }

-- | 'Nothing' leaves authentication settings database-owned. Environment mode
-- requires all three switches; provider completeness is checked by its consumers.
parseEnvironmentAuthentication :: Map Text Text -> Either Text (Maybe EnvironmentAuthentication)
parseEnvironmentAuthentication values = case M.lookup "AUTH_CONFIG_SOURCE" values of
  Nothing -> database
  Just "database" -> database
  Just "environment" -> do
    local <- boolean "LOCAL_REGISTRATION_ENABLED"
    sso <- boolean "SSO_REGISTRATION_ENABLED"
    enabled <- boolean "OIDC_ENABLED"
    pure $
      Just $
        EnvironmentAuthentication
          local
          sso
          enabled
          (maybe "Authentik" id $ optional "OIDC_PROVIDER_NAME")
          (optional "OIDC_ISSUER")
          (optional "OIDC_CLIENT_ID")
          (optional "OIDC_CLIENT_SECRET")
  _ -> Left "AUTH_CONFIG_SOURCE must be database or environment"
  where
    managedKeys =
      [ "OIDC_ENABLED",
        "LOCAL_REGISTRATION_ENABLED",
        "SSO_REGISTRATION_ENABLED",
        "OIDC_PROVIDER_NAME",
        "OIDC_ISSUER",
        "OIDC_CLIENT_ID",
        "OIDC_CLIENT_SECRET"
      ]
    optional key = M.lookup key values >>= \value -> if T.null value then Nothing else Just value
    database
      | any (maybe False (const True) . optional) managedKeys =
          Left "Set AUTH_CONFIG_SOURCE=environment to configure authentication options with environment variables"
      | otherwise = Right Nothing
    boolean key = case M.lookup key values of
      Just "true" -> Right True
      Just "false" -> Right False
      _ -> Left $ key <> " must be true or false"

-- | Apply bounded defaults and validate cross-limit ordering. Proxy CIDRs are
-- operator-supplied trust grants for interpreting forwarded client addresses.
parseRequestProtectionConfig :: Map Text Text -> Either Text RequestProtectionConfig
parseRequestProtectionConfig values = do
  ip <- rate "RATE_LIMIT_IP_PER_MINUTE" (protectionIpRate defaults)
  auth <- rate "RATE_LIMIT_AUTH_PER_IP_PER_MINUTE" (protectionAuthIpRate defaults)
  account <- rate "RATE_LIMIT_ACCOUNT_PER_MINUTE" (protectionPrincipalRate defaults)
  expensive <- rate "RATE_LIMIT_EXPENSIVE_PER_ACCOUNT_PER_MINUTE" (protectionExpensiveRate defaults)
  credential <- rate "RATE_LIMIT_CREDENTIAL_PER_MINUTE" (protectionCredentialRate defaults)
  authBody <- number "REQUEST_AUTH_BODY_MAX_BYTES" (fromIntegral $ protectionAuthBodyBytes defaults) 1048576
  body <- number "REQUEST_BODY_MAX_BYTES" (fromIntegral $ protectionBodyBytes defaults) 16777216
  bulkBody <- number "REQUEST_BULK_BODY_MAX_BYTES" (fromIntegral $ protectionBulkBodyBytes defaults) 67108864
  inFlight <- number "REQUEST_MAX_IN_FLIGHT" (protectionMaxInFlight defaults) 1024
  expensiveInFlight <- number "REQUEST_MAX_EXPENSIVE_IN_FLIGHT" (protectionMaxExpensiveInFlight defaults) 64
  keys <- number "RATE_LIMIT_MAX_KEYS" (protectionMaxKeys defaults) 1000000
  proxies <- case M.lookup "TRUSTED_PROXY_CIDRS" values of
    Nothing -> Right $ protectionTrustedProxies defaults
    Just value | T.null (T.strip value) -> Right []
    Just value -> let elements = T.splitOn "," value in
      if length elements > 32 then proxyError else traverse (cidr . T.strip) elements
  let cfg = RequestProtectionConfig ip auth account expensive credential
        (fromIntegral authBody) (fromIntegral body) (fromIntegral bulkBody)
        inFlight expensiveInFlight keys proxies
  validateRequestProtectionConfig cfg
  pure cfg
  where
    defaults = defaultRequestProtectionConfig
    number :: Text -> Int -> Int -> Either Text Int
    number key fallback upper = case M.lookup key values of
      Nothing -> Right fallback
      Just value
        | asciiDecimal value,
          Just parsed <- (readMaybe (T.unpack value) :: Maybe Integer),
          parsed >= 1, parsed <= toInteger upper -> Right $ fromInteger parsed
      _ -> Left $ key <> " is outside its permitted range"
    rate key fallback = do
      value <- number key (ratePerMinute fallback) 1000000
      pure $ RatePolicy value (min value $ rateBurst fallback)
    asciiDecimal value = not (T.null value) && T.all (\c -> c >= '0' && c <= '9') value
    proxyError = Left "TRUSTED_PROXY_CIDRS must contain at most 32 valid IPv4/IPv6 CIDRs"
    cidr value = case T.splitOn "/" value of
      [address, prefix]
        | T.all (\c -> c `elem` ("0123456789abcdefABCDEF:." :: String)) address,
          asciiDecimal prefix,
          Just ip <- readMaybe (T.unpack address),
          Just bits <- (readMaybe (T.unpack prefix) :: Maybe Integer) -> case ip of
            IPv4 v4 | bits <= 32 -> Right $ IPv4Range $ makeAddrRange v4 (fromInteger bits)
            IPv6 v6 | bits <= 128 -> Right $ IPv6Range $ makeAddrRange v6 (fromInteger bits)
            _ -> proxyError
      _ -> proxyError
