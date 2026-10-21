{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | App logout, one-use RP logout redirects and verified provider notifications.
module Jizhang.API.Logout (LogoutAPI, logoutServer, OIDCLogoutAPI, oidcLogoutServer, LogoutForm (..)) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Jizhang.API.App
import Jizhang.API.Types (AuthUser)
import qualified Jizhang.API.Types.OIDC as DTO
import Jizhang.Auth.Context
import Jizhang.Auth.Security
import qualified Jizhang.Database.Auth as A
import qualified Jizhang.Database.Schema as S
import qualified Jizhang.Database.User as U
import qualified Jizhang.OIDC as O
import Network.HTTP.Types.URI (parseQuery, renderQuery)
import Servant

-- Verb, rather than NoContentVerb, is required by servant-auth AddSetCookie.
type LogoutAPI =
  "users"
    :> "me"
    :> ( "logout" :> Verb 'POST 204 '[JSON] NoContent
           :<|> "oidc" :> "logout" :> Post '[JSON] DTO.ProviderLogoutResponse
       )

-- | Public one-use browser handoff/callback and signed provider backchannel notification.
type OIDCLogoutAPI =
  "auth"
    :> "oidc"
    :> ( "logout" :> QueryParams "token" Text :> Get '[JSON] NoContent
           :<|> "logout" :> "callback" :> QueryParams "state" Text :> QueryParams "error" Text :> Get '[JSON] NoContent
           :<|> "backchannel-logout" :> ReqBody '[FormUrlEncoded] LogoutForm :> Verb 'POST 200 '[JSON] NoContent
       )

-- Preserve raw form pairs: decoding to a map would silently collapse duplicates.
newtype LogoutForm = LogoutForm [(Text, Maybe Text)]

-- The public schema is a single required form field, not the raw-pair storage.
data LogoutFormSchema = LogoutFormSchema {logout_token :: Text} deriving (Generic)

instance Swagger.ToSchema LogoutForm where
  declareNamedSchema _ = Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions (Proxy :: Proxy LogoutFormSchema)

instance {-# OVERLAPPING #-} MimeRender FormUrlEncoded LogoutForm where
  mimeRender _ (LogoutForm fields) =
    LBS.fromStrict $
      renderQuery
        False
        [(TE.encodeUtf8 name, TE.encodeUtf8 <$> value) | (name, value) <- fields]

instance {-# OVERLAPPING #-} MimeUnrender FormUrlEncoded LogoutForm where
  mimeUnrender _ bytes = case traverse decodePair (parseQuery (LBS.toStrict bytes)) of
    Left _ -> Left "Invalid logout form"
    Right pairs -> Right (LogoutForm pairs)
    where
      decodePair (key, value) = (,) <$> TE.decodeUtf8' key <*> traverse TE.decodeUtf8' value

-- | Bind local and provider logout to the currently authenticated user session.
logoutServer :: AuthUser -> MyServer LogoutAPI
logoutServer auth = localLogout auth :<|> providerLogout auth

oidcLogoutServer :: MyServer OIDCLogoutAPI
oidcLogoutServer = logoutHandoff :<|> logoutCallback :<|> backchannelLogout

invalid :: MyHandler a
invalid = throwError $ err400 {errBody = "This logout request is invalid or expired"}

unavailable :: MyHandler a
unavailable = throwError $ err503 {errBody = "Signed out locally; provider sign-out is unavailable"}

clock :: MyHandler UTCTime
clock = asks appAuthRuntime >>= liftIO . O.authClock

-- | Revoke this session only after comparing its current account snapshot atomically.
localLogout :: AuthUser -> MyHandler NoContent
localLogout auth = do
  (user, session, identity) <- currentAccount auth
  now <- clock
  runDBSensitiveTransaction $ do
    checkAccountSnapshot now user session identity
    void $ A.revokeSession session now
  pure NoContent

-- Revoke first, independently of runtime configuration or provider availability.
-- A provider error cannot roll back this already-committed local sign-out.
providerLogout :: AuthUser -> MyHandler DTO.ProviderLogoutResponse
providerLogout auth = do
  (user, session, identity) <- currentAccount auth
  runtime <- asks appAuthRuntime
  now <- clock
  (settings, revokedSession) <- runDBSensitiveTransaction $ do
    checkAccountSnapshot now user session identity
    row <- A.getAuthSettings
    revoked <- A.revokeSession session now
    pure (row, revoked)
  providerSettings <- either (const unavailable) pure (providerSettingsFromRow runtime settings)
  unless (S._sessionMethod session == "oidc" && S._sessionOidcIssuer session == Just (O.settingsIssuer providerSettings)) unavailable
  key <- maybe unavailable pure (O.authSecretKey runtime)
  source <- maybe unavailable pure (S._sessionIdTokenEncrypted session)
  hint <-
    either (const unavailable) pure $
      decryptSecret
        key
        (TE.encodeUtf8 $ "user_sessions:" <> UUID.toText (S._sessionId session) <> ":id_token")
        source
  provider <- liftIO (O.cachedProvider runtime providerSettings) >>= either (const unavailable) pure
  endpoint <- maybe unavailable pure (O.providerEndSessionEndpoint provider)
  public <- maybe unavailable pure (O.authPublicApiUrl runtime)
  _ <- either (const unavailable) pure (O.postLogoutRedirectUrl runtime)
  ident <- liftIO UUID.nextRandom
  token <- liftIO randomToken
  initialState <- liftIO randomToken
  -- Keep the ID-token hint server-side until the browser uses its short-lived handoff;
  -- bind the ciphertext to this row and its provider destination to prevent substitution.
  encrypted <- liftIO $ encryptSecret key (logoutAad ident endpoint (O.settingsClientId providerSettings)) hint
  created <- clock
  let row =
        S.OidcLogoutState
          ident
          (tokenDigest token)
          (tokenDigest initialState)
          encrypted
          endpoint
          (O.settingsClientId providerSettings)
          (addUTCTime 60 created)
          (addUTCTime 300 created)
          Nothing
          Nothing
  -- Network discovery left a race window after revocation; issue a handoff only if
  -- settings, account identity and the already-revoked session still match the snapshot.
  runDBSensitiveTransaction $ do
    currentSettings <- A.getAuthSettings
    currentUser <- U.getUserById (S._userId user)
    currentSession <- A.getUserSession (S._sessionId session)
    currentIdentity <- A.getUserIdentity (S._userId user)
    unless
      ( currentSettings == settings
          && currentUser == Just user
          && currentIdentity == identity
          -- Compare the returned row, not a clock value PostgreSQL may have rounded.
          && currentSession == Just revokedSession
      )
      A.abortAuthTransaction
    A.cleanupExpiredAuth created
    A.insertOidcLogoutState row
  pure $ DTO.ProviderLogoutResponse $ queryUrl (O.appendBasePath public "auth/oidc/logout") [("token", token)]

logoutAad :: UUID.UUID -> Text -> Text -> ByteString
logoutAad ident endpoint client =
  TE.encodeUtf8 $
    "oidc_logout_states:"
      <> UUID.toText ident
      <> ":id_token:"
      -- The length prefix makes the endpoint/client boundary unambiguous in authenticated data.
      <> T.pack (show (T.length endpoint))
      <> ":"
      <> endpoint
      <> ":"
      <> client

queryUrl :: Text -> [(Text, Text)] -> Text
queryUrl endpoint fields =
  endpoint
    <> (if "?" `T.isInfixOf` endpoint then "&" else "?")
    <> TE.decodeUtf8 (renderQuery False [(TE.encodeUtf8 key, Just (TE.encodeUtf8 value)) | (key, value) <- fields])

single :: [Text] -> MyHandler Text
single [value] | not (T.null value) = pure value
single _ = invalid

redirect :: Text -> MyHandler a
redirect url =
  throwError $
    err303
      { errHeaders = [("Location", TE.encodeUtf8 url), ("Cache-Control", "no-store"), ("Referrer-Policy", "no-referrer")]
      }

-- | Consume the browser handoff before redirecting with the decrypted ID-token hint;
-- a fresh callback state replaces the original state for this one-use redirect.
logoutHandoff :: [Text] -> MyHandler NoContent
logoutHandoff tokens = do
  token <- single tokens
  runtime <- asks appAuthRuntime
  now <- clock
  row <- runDBSensitive (A.getOidcLogoutStateByHandoffHash (tokenDigest token)) >>= maybe invalid pure
  unless (S._logoutStateHandoffExpiresAt row > now && S._logoutStateHandoffConsumedAt row == Nothing) invalid
  key <- maybe unavailable pure (O.authSecretKey runtime)
  hint <-
    either (const unavailable) pure $
      decryptSecret
        key
        (logoutAad (S._logoutStateId row) (S._logoutStateEndpoint row) (S._logoutStateClientId row))
        (S._logoutStateIdTokenEncrypted row)
  callback <- either (const unavailable) pure (O.postLogoutRedirectUrl runtime)
  state <- liftIO randomToken
  -- Only the redirect receives this fresh state; possessing the earlier handoff token
  -- must not also authorize completing the callback.
  let consumed =
        row
          { S._logoutStateHandoffConsumedAt = Just now,
            S._logoutStateStateHash = tokenDigest state,
            S._logoutStateStateExpiresAt = addUTCTime 300 now
          }
  -- Saving against the original snapshot makes concurrent handoff attempts compete
  -- for one consumption before any response exposes the provider redirect.
  runDBSensitiveTransaction $ do
    A.saveOidcLogoutState row consumed
    A.cleanupExpiredAuth now
  redirect $
    queryUrl
      (S._logoutStateEndpoint row)
      [("client_id", S._logoutStateClientId row), ("id_token_hint", hint), ("post_logout_redirect_uri", callback), ("state", state)]

-- | Consume exactly one unexpired callback state before returning to the login page.
logoutCallback :: [Text] -> [Text] -> MyHandler NoContent
logoutCallback states errors = do
  state <- single states
  unless (null errors || (length errors == 1 && all (not . T.null) errors)) invalid
  runtime <- asks appAuthRuntime
  frontend <- maybe unavailable pure (O.authFrontendUrl runtime)
  now <- clock
  row <- runDBSensitive (A.getOidcLogoutStateByStateHash (tokenDigest state)) >>= maybe invalid pure
  unless
    ( S._logoutStateHandoffConsumedAt row /= Nothing
        && S._logoutStateStateConsumedAt row == Nothing
        && S._logoutStateStateExpiresAt row > now
    )
    invalid
  -- A provider error still completes local logout; consume its state rather than
  -- leaving a replayable callback merely because provider sign-out was refused.
  runDBSensitiveTransaction $ do
    A.consumeLogoutState row now
    A.cleanupExpiredAuth now
  redirect (frontend <> "#/login?signedOut=1")

-- | Verify the provider notification before atomically recording its replay fence
-- and revoking matching sessions/flows; repeated issuer/JTI pairs are harmless.
backchannelLogout :: LogoutForm -> MyHandler NoContent
backchannelLogout (LogoutForm fields) = do
  token <- case [value | (name, value) <- fields, name == "logout_token"] of
    [Just value] | not (T.null value) -> pure value
    _ -> invalid
  runtime <- asks appAuthRuntime
  settings <- runDBSensitive A.getAuthSettings
  -- No enabled check or secret decryption: a disabled provider still terminates
  -- existing sessions, and signature verification needs only issuer/client/JWKS.
  issuer <- maybe invalid pure (S._settingsIssuer settings)
  client <- maybe invalid pure (S._settingsClientId settings)
  proof <-
    liftIO (O.validateLogoutToken runtime (O.ProviderSettings (S._settingsRevision settings) issuer client "") token)
      >>= either (const invalid) pure
  now <- clock
  runDBSensitiveTransaction $ do
    currentSettings <- A.getAuthSettings
    unless (currentSettings == settings) A.abortAuthTransaction
    -- Protocol validation has no replay state: the JTI fence and all revocations
    -- belong to this transaction, so a failure cannot consume the event without its effects.
    existing <- A.getOidcLogoutEvent (O.logoutIssuer proof) (O.logoutJti proof)
    case existing of
      Just _ -> pure ()
      Nothing -> do
        sessions <- A.listOidcSessionsForIssuer (O.logoutIssuer proof)
        flows <- A.listPendingOidcTransactions
        -- Read and classify all domain state before the first write.
        let matchingSessions = filter (matchesSession proof) sessions
            matchingFlows = filter (matchesFlow runtime proof now) flows
        A.cleanupExpiredAuth now
        A.insertOidcLogoutEvent $
          S.OidcLogoutEvent
            (O.logoutIssuer proof)
            (O.logoutJti proof)
            (O.logoutSid proof)
            (O.logoutSubject proof)
            now
            -- Retain the replay fence longer than the protocol's accepted issuance window.
            (addUTCTime 1800 now)
        forM_ matchingSessions $ \session -> A.revokeSession session now
        forM_ matchingFlows $ \flow -> do
          -- Session revocation may already have failed a bound flow; re-read its
          -- post-write snapshot instead of comparing against the pre-write row.
          current <- A.getOidcTransaction (S._transactionId flow)
          forM_ current $ \row ->
            when (S._transactionPhase row /= "failed") $
              A.saveOidcTransaction row (row {S._transactionPhase = "failed", S._transactionResultPayloadEncrypted = Nothing})
  pure NoContent

matchesSession :: O.VerifiedLogout -> S.UserSession -> Bool
matchesSession proof session =
  S._sessionOidcIssuer session == Just (O.logoutIssuer proof)
    && matchesSubject proof (S._sessionOidcSid session) (S._sessionOidcSubject session)

-- | A supplied sid narrows matching, with sub as an extra constraint when present;
-- without sid, subject matching applies across the provider's sessions.
matchesSubject :: O.VerifiedLogout -> Maybe Text -> Maybe Text -> Bool
matchesSubject proof sid subject = case O.logoutSid proof of
  Just expected -> sid == Just expected && maybe True (\value -> subject == Just value) (O.logoutSubject proof)
  Nothing -> subject == O.logoutSubject proof

matchesFlow :: O.AuthRuntime -> O.VerifiedLogout -> UTCTime -> S.OidcTransaction -> Bool
matchesFlow runtime proof now flow =
  -- Pending flows with a verified result can still create sessions; invalidate those
  -- results too, but never infer a logout target from an unauthenticated payload.
  S._transactionCreatedAt flow <= now && case do
    key <- O.authSecretKey runtime
    payload <- S._transactionResultPayloadEncrypted flow
    either (const Nothing) Just $
      O.decryptVerifiedIdentity
        key
        (TE.encodeUtf8 $ "oidc_transactions:" <> UUID.toText (S._transactionId flow) <> ":result_payload")
        payload of
    Just identity ->
      O.verifiedIssuer identity == O.logoutIssuer proof
        && matchesSubject proof (O.verifiedSid identity) (Just (O.verifiedSubject identity))
    Nothing -> False
