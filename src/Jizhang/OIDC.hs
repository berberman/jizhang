{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The protocol boundary. No database operations or application sessions live here.
-- Secrets deliberately have neither Show nor JSON instances.
module Jizhang.OIDC
  ( AuthRuntime (..),
    newAuthRuntime,
    runtimeReady,
    ProtocolError (..),
    protocolErrorCode,
    ProviderSettings (..),
    VerifiedProvider,
    providerConfiguration,
    providerEndSessionEndpoint,
    providerBackchannelLogoutSupported,
    providerBackchannelLogoutSessionSupported,
    discoverProvider,
    cachedProvider,
    invalidateProviderCache,
    validateBaseUrl,
    appendBasePath,
    callbackUrl,
    backchannelLogoutUrl,
    postLogoutRedirectUrl,
    authorizationUrl,
    providerLogoutUrl,
    VerifiedOidcIdentity,
    verifiedIssuer,
    verifiedSubject,
    verifiedSid,
    verifiedIssuedAt,
    verifiedAuthTime,
    verifiedReauthenticatedAt,
    verifiedIdToken,
    verifiedProfile,
    ProfileHints (..),
    exchangeAuthorizationCode,
    VerifiedLogout (..),
    validateLogoutToken,
    encryptVerifiedIdentity,
    decryptVerifiedIdentity,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, toLower)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock.POSIX
import Jizhang.Auth.Security (SecretKey, decryptSecret, encryptSecret, parseSecretKey, secureEqual, tokenDigest)
import qualified Jose.Jwa as Jwa
import qualified Jose.Jwk as Jwk
import qualified Jose.Jwt as Jwt
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderQuery, urlEncode)
import Network.URI (URI, parseURI, uriAuthority, uriFragment, uriPort, uriQuery, uriRegName, uriScheme, uriUserInfo)
import System.Timeout (timeout)
import qualified Web.OIDC.Client as OIDC
import qualified Web.OIDC.Client.Discovery.Provider as P

-- These fixed categories are safe to log; never render provider exceptions.
data ProtocolError = InvalidResponse | ProviderUnavailable | InvalidConfiguration
  deriving (Eq, Show)

instance Exception ProtocolError

protocolErrorCode :: ProtocolError -> Text
protocolErrorCode InvalidResponse = "invalid_response"
protocolErrorCode ProviderUnavailable = "provider_unavailable"
protocolErrorCode InvalidConfiguration = "configuration_changed"

-- | Process-owned transport, validated public URLs, encryption key and provider cache.
-- The clock is injectable; persisted settings and one-time proof consumption live elsewhere.
data AuthRuntime = AuthRuntime
  { authPublicApiUrl :: !(Maybe Text),
    authFrontendUrl :: !(Maybe Text),
    authSecretKey :: !(Maybe SecretKey),
    authManager :: !Manager,
    authProviderCache :: !(MVar (Maybe CacheEntry)),
    authClock :: IO UTCTime
  }

data CacheEntry = CacheEntry !Int64 !Text !Text !UTCTime !VerifiedProvider

-- | Validate deployment URLs/key and require the frontend origin to be same-origin
-- or explicitly allowed. Missing values defer availability checks to 'runtimeReady'.
newAuthRuntime :: Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> IO AuthRuntime
newAuthRuntime public frontend key origins = do
  p <- traverse (configured "PUBLIC_API_URL" . validateBaseUrl False) public
  f <- traverse (configured "FRONTEND_URL" . validateBaseUrl True) frontend
  k <- traverse (configured "OIDC_SECRET_KEY" . parseSecretKey) key
  case (p, f) of
    (Just api, Just web) -> do
      let origin = urlOrigin web
      unless
        (origin == urlOrigin api || any ((== origin) . urlOrigin) origins)
        (ioError (userError "FRONTEND_URL origin must equal PUBLIC_API_URL origin or be explicitly listed in CLIENT_ORIGINS"))
    _ -> pure ()
  manager <-
    newManager
      tlsManagerSettings
        { managerRetryableException = const False,
          managerResponseTimeout = responseTimeoutMicro deadline
        }
  cache <- newMVar Nothing
  pure (AuthRuntime p f k manager cache getCurrentTime)
  where
    configured name = either (const (ioError (userError (name <> " is invalid")))) pure

-- | Check deployment prerequisites only, not provider discovery or database settings.
runtimeReady :: AuthRuntime -> Either Text ()
runtimeReady runtime = do
  unless (isJust (authPublicApiUrl runtime)) (Left "Configure PUBLIC_API_URL to enable OIDC and invitations")
  unless (isJust (authFrontendUrl runtime)) (Left "Configure FRONTEND_URL to enable OIDC and invitations")
  unless (isJust (authSecretKey runtime)) (Left "Configure OIDC_SECRET_KEY to enable OIDC and invitations")

-- | This parser never echoes its input (which may contain accidentally pasted secrets).
validateBaseUrl :: Bool -> Text -> Either Text Text
validateBaseUrl frontend raw = do
  uri <- safeUri raw
  unless (null (uriQuery uri)) (Left "URL must not contain a query")
  unless (not frontend || "/" `T.isSuffixOf` raw) (Left "Frontend URL must end with /")
  pure raw

safeUri :: Text -> Either Text URI
safeUri raw = do
  unless (not (T.null raw) && not (T.any (\c -> isControl c || c == ' ' || c == '\\') raw)) (Left "Invalid URL")
  uri <- maybe (Left "URL must be absolute") Right (parseURI (T.unpack raw))
  authority <- maybe (Left "URL must have an authority") Right (uriAuthority uri)
  let hostName = uriRegName authority
      scheme = uriScheme uri
  unless (null (uriUserInfo authority) && not (null hostName) && '*' `notElem` hostName && null (uriFragment uri)) (Left "Invalid URL authority or fragment")
  unless (scheme == "https:" || (scheme == "http:" && hostName `elem` ["127.0.0.1", "[::1]", "localhost"])) (Left "URL requires HTTPS except literal loopback hosts")
  unless (null (uriPort authority) || validPort (uriPort authority)) (Left "Invalid port")
  pure uri
  where
    validPort (':' : digits) = case reads digits :: [(Int, String)] of
      [(n, "")] -> n > 0 && n <= 65535
      _ -> False
    validPort _ = False

urlOrigin :: Text -> Maybe (String, String, String)
urlOrigin raw = do
  uri <- either (const Nothing) Just (safeUri raw)
  authority <- uriAuthority uri
  let scheme = uriScheme uri
      portNumber = case uriPort authority of
        "" -> if scheme == "https:" then ":443" else ":80"
        value -> value
  pure (scheme, map toLower (uriRegName authority), portNumber)

-- Only application-owned relative suffixes are passed to this helper.
appendBasePath :: Text -> Text -> Text
appendBasePath base suffix = T.dropWhileEnd (== '/') base <> "/" <> T.dropWhile (== '/') suffix

runtimePath :: AuthRuntime -> Text -> Either Text Text
runtimePath runtime suffix = maybe (Left "Configure PUBLIC_API_URL") (Right . (`appendBasePath` suffix)) (authPublicApiUrl runtime)

callbackUrl, backchannelLogoutUrl, postLogoutRedirectUrl :: AuthRuntime -> Either Text Text
callbackUrl runtime = runtimePath runtime "auth/oidc/callback"
backchannelLogoutUrl runtime = runtimePath runtime "auth/oidc/backchannel-logout"
postLogoutRedirectUrl runtime = runtimePath runtime "auth/oidc/logout/callback"

-- | A caller-owned settings snapshot; its revision participates in cache identity.
-- This includes a decrypted client credential and must not be rendered in diagnostics.
data ProviderSettings = ProviderSettings
  { settingsRevision :: !Int64,
    settingsIssuer :: !Text,
    settingsClientId :: !Text,
    settingsClientSecret :: !Text
  }

-- | Discovery accepted under this module's issuer, endpoint and algorithm policy.
data VerifiedProvider = VerifiedProvider
  { internalProvider :: !OIDC.Provider,
    providerEndSessionEndpoint :: !(Maybe Text),
    providerBackchannelLogoutSupported :: !Bool,
    providerBackchannelLogoutSessionSupported :: !Bool
  }

providerConfiguration :: VerifiedProvider -> OIDC.Configuration
providerConfiguration = OIDC.configuration . internalProvider

-- | Discard cached metadata after settings changes; in-flight users retain their value.
invalidateProviderCache :: AuthRuntime -> IO ()
invalidateProviderCache runtime = modifyMVar_ (authProviderCache runtime) (const (pure Nothing))

-- | Serialize cache access and discovery under one lock; successful entries last
-- five minutes and match revision, issuer and client. Failures leave no cached entry.
cachedProvider :: AuthRuntime -> ProviderSettings -> IO (Either ProtocolError VerifiedProvider)
cachedProvider runtime settings = modifyMVar (authProviderCache runtime) $ \entry -> do
  now <- authClock runtime
  case entry of
    Just (CacheEntry revision issuer client expires provider)
      | revision == settingsRevision settings
          && issuer == settingsIssuer settings
          && client == settingsClientId settings
          && now < expires ->
          pure (entry, Right provider)
    _ -> do
      result <- discoverProvider runtime settings
      pure
        ( case result of
            Right provider -> (Just (CacheEntry (settingsRevision settings) (settingsIssuer settings) (settingsClientId settings) (addUTCTime 300 now) provider), result)
            Left _ -> (Nothing, result)
        )

-- | Explicit connection tests bypass the cache, so drafts never poison a saved revision.
-- Discovery requires exact issuer equality, same-origin endpoints, RS256 and S256.
discoverProvider :: AuthRuntime -> ProviderSettings -> IO (Either ProtocolError VerifiedProvider)
discoverProvider runtime settings = runExceptT $ do
  _ <- fromEither InvalidConfiguration (validateBaseUrl False (settingsIssuer settings))
  require InvalidConfiguration (not (T.null (T.strip (settingsClientId settings))))
  metadata <- fetchJson runtime (appendBasePath (settingsIssuer settings) ".well-known/openid-configuration") id
  (configuration, pkce, endSession, backchannel, backchannelSession) <- parseValue InvalidConfiguration metadata $ withObject "discovery" $ \o ->
    (,,,,)
      <$> parseJSON (Object o)
      <*> o .: "code_challenge_methods_supported"
      <*> o .:? "end_session_endpoint"
      <*> o .:? "backchannel_logout_supported" .!= False
      <*> o .:? "backchannel_logout_session_supported" .!= False
  -- Exact issuer equality keeps discovery aliases from changing the identity namespace.
  require InvalidConfiguration (OIDC.issuer configuration == settingsIssuer settings)
  require InvalidConfiguration ("code" `elem` OIDC.responseTypesSupported configuration)
  require InvalidConfiguration ("S256" `elem` (pkce :: [Text]))
  require InvalidConfiguration (P.JwsAlgJson Jwa.RS256 `elem` OIDC.idTokenSigningAlgValuesSupported configuration)
  require InvalidConfiguration (maybe True (elem "client_secret_basic") (OIDC.tokenEndpointAuthMethodsSupported configuration))
  mapM_
    (validateEndpoint (settingsIssuer settings))
    ( [OIDC.authorizationEndpoint configuration, OIDC.tokenEndpoint configuration, OIDC.jwksUri configuration]
        <> maybe [] (: []) (OIDC.userinfoEndpoint configuration)
        <> maybe [] (: []) endSession
    )
  -- Validate every discovered destination before any credential-bearing request;
  -- advertised algorithms cannot widen the verifier beyond this application's RS256 policy.
  keys <- fetchKeys runtime (OIDC.jwksUri configuration)
  let restricted = configuration {OIDC.idTokenSigningAlgValuesSupported = [P.JwsAlgJson Jwa.RS256]}
  pure (VerifiedProvider (OIDC.Provider restricted keys) endSession backchannel backchannelSession)

validateEndpoint :: Text -> Text -> ExceptT ProtocolError IO ()
validateEndpoint issuer endpoint = do
  _ <- fromEither InvalidConfiguration (safeUri endpoint)
  require InvalidConfiguration (urlOrigin issuer == urlOrigin endpoint)

fetchKeys :: AuthRuntime -> Text -> ExceptT ProtocolError IO [Jwk.Jwk]
fetchKeys runtime uri = do
  value <- fetchJson runtime uri id
  keySet <- parseValue InvalidConfiguration value parseJSON
  -- Never let private, symmetric or encryption-only keys enter the signature key set.
  let keys = filter publicSigningKey (Jwk.keys keySet)
  require InvalidConfiguration (not (null keys))
  pure keys
  where
    publicSigningKey key@Jwk.RsaPublicJwk {} = Jwk.jwkUse key /= Just Jwk.Enc
    publicSigningKey _ = False

deadline :: Int
deadline = 10000000

-- Both the total request deadline and the streaming body bound apply to every endpoint.
fetchJson :: AuthRuntime -> Text -> (Request -> Request) -> ExceptT ProtocolError IO Value
fetchJson runtime uri alter = do
  result <-
    liftIO $
      ( timeout deadline $ do
          request <- parseRequest (T.unpack uri)
          let bounded =
                (alter request)
                  -- A redirect could escape the checked origin with a code, secret or bearer token.
                  { redirectCount = 0,
                    responseTimeout = responseTimeoutMicro deadline,
                    checkResponse = \_ _ -> pure (),
                    cookieJar = Nothing
                  }
          withResponse bounded (authManager runtime) $ \response -> do
            let status = statusCode (responseStatus response)
            unless (status >= 200 && status < 300) (throwIO ProviderUnavailable)
            body <- consume 0 [] (responseBody response)
            Right <$> either (const (throwIO InvalidResponse)) pure (eitherDecodeStrict' body)
      )
        `catch` (\(_ :: HttpException) -> pure (Just (Left ProviderUnavailable)))
        `catch` (\(err :: ProtocolError) -> pure (Just (Left err)))
  case result of
    Nothing -> throwE ProviderUnavailable
    Just value -> either throwE pure value
  where
    consume size chunks reader = do
      -- Bound bytes while streaming: checking after concatenation would still allow unbounded allocation.
      chunk <- brRead reader
      let next = size + BS.length chunk
      when (next > 1048576) (throwIO InvalidResponse)
      if BS.null chunk then pure (BS.concat (reverse chunks)) else consume next (chunk : chunks) reader

fromEither :: ProtocolError -> Either a b -> ExceptT ProtocolError IO b
fromEither err = either (const (throwE err)) pure

parseValue :: ProtocolError -> Value -> (Value -> Parser a) -> ExceptT ProtocolError IO a
parseValue err value parser = fromEither err (parseEither parser value)

require :: ProtocolError -> Bool -> ExceptT ProtocolError IO ()
require err condition = unless condition (throwE err)

withQuery :: Text -> [(ByteString, ByteString)] -> Text
withQuery endpoint fields = endpoint <> separator <> TE.decodeUtf8 (renderQuery False [(key, Just value) | (key, value) <- fields])
  where
    separator = if "?" `T.isInfixOf` endpoint then "&" else "?"

-- | Build the provider redirect from caller-generated state, nonce and S256 challenge.
-- Their storage, browser binding, expiry and one-time consumption are caller duties.
authorizationUrl :: VerifiedProvider -> ProviderSettings -> Text -> Text -> Text -> Text -> Bool -> Text
authorizationUrl provider settings redirect state nonce challenge reauthenticate =
  withQuery
    (OIDC.authorizationEndpoint (providerConfiguration provider))
    ( [ ("response_type", "code"),
        ("client_id", TE.encodeUtf8 (settingsClientId settings)),
        ("redirect_uri", TE.encodeUtf8 redirect),
        ("scope", "openid profile email"),
        ("state", TE.encodeUtf8 state),
        ("nonce", TE.encodeUtf8 nonce),
        ("code_challenge", TE.encodeUtf8 challenge),
        ("code_challenge_method", "S256")
      ]
        -- A positive age activates freshness checks in providers that treat zero as unset.
        <> if reauthenticate then [("prompt", "login"), ("max_age", "1")] else []
    )

-- | Build an RP-initiated logout redirect, failing if discovery lacks the endpoint.
-- The caller supplies a decrypted ID-token hint and tracks the logout state proof.
providerLogoutUrl :: VerifiedProvider -> Text -> Text -> Text -> Text -> Either ProtocolError Text
providerLogoutUrl provider client redirect state hint = do
  endpoint <- maybe (Left InvalidConfiguration) Right (providerEndSessionEndpoint provider)
  pure (withQuery endpoint [("client_id", TE.encodeUtf8 client), ("post_logout_redirect_uri", TE.encodeUtf8 redirect), ("state", TE.encodeUtf8 state), ("id_token_hint", TE.encodeUtf8 hint)])

-- | Provider-supplied display hints, not keys for matching local accounts.
data ProfileHints = ProfileHints
  { profileUsername :: !(Maybe Text),
    profileDisplayName :: !(Maybe Text),
    profileEmail :: !(Maybe Text),
    profileEmailVerified :: !Bool
  }
  deriving (Eq, Show)

-- | A signature/claim-checked identity, or one restored from authenticated persistence.
-- The private constructor keeps unverified profile JSON from becoming login authority.
data VerifiedOidcIdentity = VerifiedOidcIdentity
  { verifiedIssuer :: !Text,
    verifiedSubject :: !Text,
    verifiedSid :: !(Maybe Text),
    verifiedIssuedAt :: !UTCTime,
    verifiedAuthTime :: !(Maybe UTCTime),
    verifiedReauthenticatedAt :: !(Maybe UTCTime),
    verifiedIdToken :: !Text,
    verifiedProfile :: !ProfileHints
  }

-- Trusted persistence boundary: the constructor remains private and decoding is
-- possible only after authenticating the row-bound encrypted envelope.
encryptVerifiedIdentity :: SecretKey -> ByteString -> VerifiedOidcIdentity -> IO Text
encryptVerifiedIdentity key aad identity =
  encryptSecret key aad $
    TE.decodeUtf8 $
      LBS.toStrict $
        encode $
          object
            [ "issuer" .= verifiedIssuer identity,
              "subject" .= verifiedSubject identity,
              "sid" .= verifiedSid identity,
              "issuedAt" .= verifiedIssuedAt identity,
              "authTime" .= verifiedAuthTime identity,
              "reauthenticatedAt" .= verifiedReauthenticatedAt identity,
              "idToken" .= verifiedIdToken identity,
              "username" .= profileUsername profile,
              "displayName" .= profileDisplayName profile,
              "email" .= profileEmail profile,
              "emailVerified" .= profileEmailVerified profile
            ]
  where
    profile = verifiedProfile identity

-- | Restore a previously verified identity using the same row/purpose AAD.
-- This authenticates storage, not current token validity or proof non-reuse.
decryptVerifiedIdentity :: SecretKey -> ByteString -> Text -> Either Text VerifiedOidcIdentity
decryptVerifiedIdentity key aad encrypted = do
  plaintext <- decryptSecret key aad encrypted
  value <- either (const (Left "Invalid authentication payload")) Right (eitherDecodeStrict' (TE.encodeUtf8 plaintext))
  either (const (Left "Invalid authentication payload")) Right $
    parseEither
      ( withObject "identity payload" $ \o -> do
          profile <- ProfileHints <$> o .: "username" <*> o .: "displayName" <*> o .: "email" <*> o .: "emailVerified"
          VerifiedOidcIdentity
            <$> o .: "issuer"
            <*> o .: "subject"
            <*> o .: "sid"
            <*> o .: "issuedAt"
            <*> o .: "authTime"
            <*> o .: "reauthenticatedAt"
            <*> o .: "idToken"
            <*> pure profile
      )
      value

data IdentityClaims = IdentityClaims
  { claimIssuer :: !Text,
    claimSubject :: !Text,
    claimAudience :: ![Text],
    claimAzp :: !(Maybe Text),
    claimExpiry :: !UTCTime,
    claimIssued :: !UTCTime,
    claimNotBefore :: !(Maybe UTCTime),
    claimNonce :: !Text,
    claimAtHash :: !(Maybe Text),
    claimSid :: !(Maybe Text),
    claimAuthTime :: !(Maybe UTCTime),
    claimProfile :: !ProfileHints
  }

numericDate :: Object -> Key -> Parser UTCTime
numericDate claims key = posixSecondsToUTCTime . fromInteger <$> (claims .: key :: Parser Integer)

optionalDate :: Object -> Key -> Parser (Maybe UTCTime)
optionalDate claims key = fmap (posixSecondsToUTCTime . fromInteger) <$> (claims .:? key :: Parser (Maybe Integer))

parseAudience :: Object -> Parser [Text]
parseAudience o = o .: "aud" <|> ((: []) <$> o .: "aud")

parseProfile :: Object -> Parser ProfileHints
parseProfile o = ProfileHints <$> o .:? "preferred_username" <*> o .:? "name" <*> o .:? "email" <*> o .:? "email_verified" .!= False

parseIdentity :: Value -> Parser IdentityClaims
parseIdentity = withObject "ID claims" $ \o ->
  IdentityClaims
    <$> o .: "iss"
    <*> o .: "sub"
    <*> parseAudience o
    <*> o .:? "azp"
    <*> numericDate o "exp"
    <*> numericDate o "iat"
    <*> optionalDate o "nbf"
    <*> o .: "nonce"
    <*> o .:? "at_hash"
    <*> o .:? "sid"
    <*> optionalDate o "auth_time"
    <*> parseProfile o

-- Multiple audiences require an explicit authorized party matching this client.
validateAudience :: Text -> [Text] -> Maybe Text -> Bool
validateAudience client audience azp =
  client `elem` audience
    && maybe (length audience == 1) (== client) azp

-- | Check the stored nonce digest; reauthentication checks auth_time against creation.
-- Pass the transaction creation time, or Nothing for login; callers consume its state.
exchangeAuthorizationCode :: AuthRuntime -> ProviderSettings -> Text -> Text -> Text -> ByteString -> Maybe UTCTime -> IO (Either ProtocolError VerifiedOidcIdentity)
exchangeAuthorizationCode runtime settings redirect code verifier expectedNonce reauthCreated = runExceptT $ do
  provider <- ExceptT (cachedProvider runtime settings)
  require InvalidResponse (not (T.null code) && validVerifier verifier)
  -- This POST consumes a provider code; a later key rotation must not repeat the exchange.
  tokenBody <-
    fetchJson runtime (OIDC.tokenEndpoint (providerConfiguration provider)) $
      -- OAuth Basic credentials are form-encoded individually before HTTP Basic encoding.
      applyBasicAuth (formEncode (settingsClientId settings)) (formEncode (settingsClientSecret settings))
        . urlEncodedBody [("grant_type", "authorization_code"), ("code", TE.encodeUtf8 code), ("redirect_uri", TE.encodeUtf8 redirect), ("code_verifier", TE.encodeUtf8 verifier)]
  (access, rawToken) <- parseValue InvalidResponse tokenBody $ withObject "token response" $ \o -> do
    tokenType <- o .: "token_type"
    unless (T.toCaseFold tokenType == "bearer") (fail "Invalid token type")
    access <- o .: "access_token"
    token <- o .: "id_token"
    unless (not (T.null access) && not (T.null token)) (fail "Missing tokens")
    pure (access, token)
  providerWithKeys <- refreshUnknownKid runtime settings provider rawToken
  -- The code has already been exchanged: only signature validation can refresh keys.
  let validationClient =
        OIDC.setCredentials
          (TE.encodeUtf8 (settingsClientId settings))
          (TE.encodeUtf8 (settingsClientSecret settings))
          (TE.encodeUtf8 redirect)
          (OIDC.newOIDC (internalProvider providerWithKeys))
  decoded <-
    liftIO $
      (Right <$> (OIDC.validateIdToken validationClient (Jwt.Jwt (TE.encodeUtf8 rawToken)) :: IO (OIDC.IdTokenClaims Value)))
        `catch` (\(_ :: OIDC.OpenIdException) -> pure (Left InvalidResponse))
  claims <- either throwE (\verified -> parseValue InvalidResponse (OIDC.otherClaims verified) parseIdentity) decoded
  -- Only signature-checked claims reach application checks; untrusted JWT JSON never grants identity.
  now <- liftIO (authClock runtime)
  require InvalidResponse (claimIssuer claims == settingsIssuer settings && not (T.null (claimSubject claims)))
  require InvalidResponse (maybe True (not . T.null) (claimSid claims))
  require InvalidResponse (validateAudience (settingsClientId settings) (claimAudience claims) (claimAzp claims))
  require InvalidResponse (claimExpiry claims > now && claimExpiry claims > claimIssued claims && claimIssued claims <= addUTCTime 60 now)
  require InvalidResponse (maybe True (<= addUTCTime 60 now) (claimNotBefore claims))
  -- Bind this response to the caller's stored nonce digest without recovering that nonce.
  -- Matching the nonce does not consume the caller's transaction or browser proof.
  require InvalidResponse (secureEqual expectedNonce (tokenDigest (claimNonce claims)))
  -- When supplied, at_hash binds the bearer token to this signed ID token, not just the token response.
  require InvalidResponse (maybe True (secureEqual (TE.encodeUtf8 (accessHash access)) . TE.encodeUtf8) (claimAtHash claims))
  case reauthCreated of
    Nothing -> pure ()
    -- A newly issued token can describe an old login: freshness needs signed auth_time, not iat.
    Just created -> require InvalidResponse (maybe False (\t -> t >= addUTCTime (-60) created && t <= addUTCTime 60 now) (claimAuthTime claims))
  hints <- case OIDC.userinfoEndpoint (providerConfiguration providerWithKeys) of
    Nothing -> pure (claimProfile claims)
    Just endpoint -> do
      info <- fetchJson runtime endpoint (applyBearerAuth (TE.encodeUtf8 access))
      (subject, profile) <- parseValue InvalidResponse info $ withObject "userinfo" $ \o -> (,) <$> o .: "sub" <*> parseMergedProfile (claimProfile claims) o
      -- UserInfo can enrich display hints only for the subject authenticated by the ID token.
      require InvalidResponse (subject == claimSubject claims)
      pure profile
  pure
    ( VerifiedOidcIdentity
        (claimIssuer claims)
        (claimSubject claims)
        (claimSid claims)
        (claimIssued claims)
        (claimAuthTime claims)
        -- Ordinary login must not manufacture a reusable proof of explicit reauthentication.
        (if isJust reauthCreated then claimAuthTime claims else Nothing)
        rawToken
        hints
    )
  where
    formEncode = urlEncode True . TE.encodeUtf8

validVerifier :: Text -> Bool
validVerifier value = T.length value == 43 && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '-' || c == '_') value

accessHash :: Text -> Text
-- RS256 uses SHA-256: at_hash is the base64url-encoded left half of that digest.
accessHash = TE.decodeUtf8 . B64URL.encodeUnpadded . BS.take 16 . tokenDigest

-- Preserve absent fields, but a supplied email without supplied verification is unverified.
parseMergedProfile :: ProfileHints -> Object -> Parser ProfileHints
parseMergedProfile old o = do
  username <- if KM.member "preferred_username" o then o .:? "preferred_username" else pure (profileUsername old)
  name <- if KM.member "name" o then o .:? "name" else pure (profileDisplayName old)
  email <- if KM.member "email" o then o .:? "email" else pure (profileEmail old)
  verified <-
    if KM.member "email_verified" o
      then o .:? "email_verified" .!= False
      else pure (not (KM.member "email" o) && profileEmailVerified old)
  pure (ProfileHints username name email verified)

-- Inspect only the protected header to decide whether a single JWKS refresh is needed.
-- It is not a source of trusted claims or endpoint URLs.
refreshUnknownKid :: AuthRuntime -> ProviderSettings -> VerifiedProvider -> Text -> ExceptT ProtocolError IO VerifiedProvider
refreshUnknownKid runtime settings provider token = do
  header <- case BS.split 46 (TE.encodeUtf8 token) of
    [h, _, _] -> fromEither InvalidResponse (B64URL.decodeUnpadded h >>= eitherDecodeStrict')
    _ -> throwE InvalidResponse
  kid <- parseValue InvalidResponse header $ withObject "JWT header" $ \o -> do
    algorithm <- o .: "alg" :: Parser Text
    -- Reject alternate payload/header semantics before either verifier sees the token.
    unless (algorithm == "RS256" && not (KM.member "crit" o) && not (KM.member "b64" o)) (fail "Unsupported JWT")
    o .:? "kid"
  let keys = OIDC.jwkSet (internalProvider provider)
      known = maybe True (\key -> any ((== Just (String key)) . fmap toJSON . Jwk.jwkId) keys) kid
  -- Missing kid uses the current set; only an unknown explicit kid triggers one fetch,
  -- never a retry loop driven by a bad signature or provider-controlled header URL.
  if known
    then pure provider
    else do
      fresh <- fetchKeys runtime (OIDC.jwksUri (providerConfiguration provider))
      let updated = provider {internalProvider = (internalProvider provider) {OIDC.jwkSet = fresh}}
      now <- liftIO (authClock runtime)
      -- The key fetch ran outside this lock; do not overwrite a newer settings revision's cache.
      liftIO $ modifyMVar_ (authProviderCache runtime) $ \entry -> pure $ case entry of
        Just (CacheEntry revision issuer client _ _)
          | revision == settingsRevision settings && issuer == settingsIssuer settings && client == settingsClientId settings ->
              Just (CacheEntry revision issuer client (addUTCTime 300 now) updated)
        _ -> entry
      pure updated

-- | Validated logout targeting data. The caller records the JTI to detect replay
-- and revokes matching sessions; validation alone changes no application state.
data VerifiedLogout = VerifiedLogout
  { logoutIssuer :: !Text,
    logoutJti :: !Text,
    logoutSid :: !(Maybe Text),
    logoutSubject :: !(Maybe Text),
    logoutIssuedAt :: !UTCTime
  }
  deriving (Eq, Show)

data LogoutClaims = LogoutClaims !Text ![Text] !(Maybe Text) !UTCTime !(Maybe UTCTime) !Text !(Maybe Text) !(Maybe Text)

parseLogout :: Value -> Parser LogoutClaims
parseLogout = withObject "logout claims" $ \o -> do
  -- The event and forbidden nonce distinguish logout assertions from login ID tokens.
  unless (not (KM.member "nonce" o)) (fail "Logout nonce forbidden")
  events <- o .: "events"
  case KM.lookup "http://schemas.openid.net/event/backchannel-logout" events of
    Just (Object event) | KM.null event -> pure ()
    _ -> fail "Missing logout event"
  LogoutClaims
    <$> o .: "iss"
    <*> parseAudience o
    <*> o .:? "azp"
    <*> numericDate o "iat"
    <*> optionalDate o "exp"
    <*> o .: "jti"
    <*> o .:? "sid"
    <*> o .:? "sub"

-- | Check RS256, issuer/audience, logout event and a bounded issuance window.
-- Require a nonempty session or subject target; expiry is checked when supplied.
validateLogoutToken :: AuthRuntime -> ProviderSettings -> Text -> IO (Either ProtocolError VerifiedLogout)
validateLogoutToken runtime settings token = runExceptT $ do
  cached <- ExceptT (cachedProvider runtime settings)
  provider <- refreshUnknownKid runtime settings cached token
  result <- liftIO (Jwt.decode (OIDC.jwkSet (internalProvider provider)) (Just (Jwt.JwsEncoding Jwa.RS256)) (TE.encodeUtf8 token))
  payload <- case result of
    Right (Jwt.Jws (_, body)) -> pure body
    _ -> throwE InvalidResponse
  value <- fromEither InvalidResponse (eitherDecodeStrict' payload)
  -- Parse targeting data only after signature verification; this function neither records
  -- the JTI nor revokes sessions, so its caller must make those mutations atomic.
  LogoutClaims issuer audience azp issued expiry jti sid subject <- parseValue InvalidResponse value parseLogout
  now <- liftIO (authClock runtime)
  require InvalidResponse (issuer == settingsIssuer settings && validateAudience (settingsClientId settings) audience azp)
  -- exp is optional for logout tokens; the bounded iat window still limits their acceptance.
  require InvalidResponse (issued >= addUTCTime (-300) now && issued <= addUTCTime 60 now)
  require InvalidResponse (maybe True (\t -> t > now && t > issued) expiry)
  require InvalidResponse (not (T.null jti) && maybe True (not . T.null) sid && maybe True (not . T.null) subject && (isJust sid || isJust subject))
  pure (VerifiedLogout issuer jti sid subject issued)
