{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authentication persistence without token verification or cryptography.
-- Callers use the sensitive runner and own Serializable transactions for snapshot writes.
-- Conflicts throw so the enclosing transaction rolls back instead of committing a rejection.
module Jizhang.Database.Auth where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runDeleteReturningList, runUpdateReturningList)
import Database.Beam.Postgres
import qualified Jizhang.Database.Schema as S

-- | An expected row, identity or phase no longer matches. Do not catch this
-- inside the transaction and commit earlier writes; abort the whole mutation.
data AuthTransactionConflict = AuthTransactionConflict deriving (Eq, Show)

instance Exception AuthTransactionConflict

abortAuthTransaction :: Pg a
abortAuthTransaction = liftIO (throwIO AuthTransactionConflict)

-- | Persist caller-issued session evidence verbatim. Expiry, account eligibility,
-- token encryption and authentication method validation are not checked here.
insertUserSession :: S.UserSession -> Pg ()
insertUserSession row = runInsert $ insert (S._userSessions S.jizhangDb) (insertValues [row])

getUserSession :: UUID -> Pg (Maybe S.UserSession)
getUserSession rowId = runSelectReturningOne $ select $ do
  row <- all_ (S._userSessions S.jizhangDb)
  guard_ (S._sessionId row ==. val_ rowId)
  pure row

-- | Compare the complete expected snapshot before replacing; stale writes abort.
-- The snapshot is database-read and checked before writing in a Serializable
-- transaction. Check write cardinality only: TIMESTAMPTZ rounds runtime values.
saveUserSession :: S.UserSession -> S.UserSession -> Pg ()
saveUserSession expected replacement = do
  unless (S._sessionId expected == S._sessionId replacement) abortAuthTransaction
  current <- getUserSession (S._sessionId expected)
  -- This read is a snapshot check, not a row lock; Serializable must reject concurrent replacements.
  unless (current == Just expected) abortAuthTransaction
  rows <- runUpdateReturningList $ save (S._userSessions S.jizhangDb) replacement
  -- Compare cardinality, not returned timestamps: PostgreSQL may round the replacement's precision.
  case rows of
    [_] -> pure ()
    _ -> abortAuthTransaction

insertAccountInvitation :: S.AccountInvitation -> Pg ()
insertAccountInvitation row = runInsert $ insert (S._accountInvitations S.jizhangDb) (insertValues [row])

getAccountInvitation :: UUID -> Pg (Maybe S.AccountInvitation)
getAccountInvitation rowId = runSelectReturningOne $ select $ do
  row <- all_ (S._accountInvitations S.jizhangDb)
  guard_ (S._invitationId row ==. val_ rowId)
  pure row

-- | Compare the complete expected snapshot before replacing; stale writes abort.
saveAccountInvitation :: S.AccountInvitation -> S.AccountInvitation -> Pg ()
saveAccountInvitation expected replacement = do
  unless (S._invitationId expected == S._invitationId replacement) abortAuthTransaction
  current <- getAccountInvitation (S._invitationId expected)
  unless (current == Just expected) abortAuthTransaction
  rows <- runUpdateReturningList $ save (S._accountInvitations S.jizhangDb) replacement
  case rows of
    [_] -> pure ()
    _ -> abortAuthTransaction

insertOidcTransaction :: S.OidcTransaction -> Pg ()
insertOidcTransaction row = runInsert $ insert (S._oidcTransactions S.jizhangDb) (insertValues [row])

getOidcTransaction :: UUID -> Pg (Maybe S.OidcTransaction)
getOidcTransaction rowId = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcTransactions S.jizhangDb)
  guard_ (S._transactionId row ==. val_ rowId)
  pure row

-- | Compare the complete expected snapshot before replacing; stale writes abort.
saveOidcTransaction :: S.OidcTransaction -> S.OidcTransaction -> Pg ()
saveOidcTransaction expected replacement = do
  unless (S._transactionId expected == S._transactionId replacement) abortAuthTransaction
  current <- getOidcTransaction (S._transactionId expected)
  unless (current == Just expected) abortAuthTransaction
  rows <- runUpdateReturningList $ save (S._oidcTransactions S.jizhangDb) replacement
  case rows of
    [_] -> pure ()
    _ -> abortAuthTransaction

insertOidcLogoutEvent :: S.OidcLogoutEvent -> Pg ()
insertOidcLogoutEvent row = runInsert $ insert (S._oidcLogoutEvents S.jizhangDb) (insertValues [row])

getOidcLogoutEvent :: Text -> Text -> Pg (Maybe S.OidcLogoutEvent)
getOidcLogoutEvent issuer jti = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcLogoutEvents S.jizhangDb)
  guard_ (S._logoutEventIssuer row ==. val_ issuer &&. S._logoutEventJti row ==. val_ jti)
  pure row

insertOidcLogoutState :: S.OidcLogoutState -> Pg ()
insertOidcLogoutState row = runInsert $ insert (S._oidcLogoutStates S.jizhangDb) (insertValues [row])

getOidcLogoutState :: UUID -> Pg (Maybe S.OidcLogoutState)
getOidcLogoutState rowId = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcLogoutStates S.jizhangDb)
  guard_ (S._logoutStateId row ==. val_ rowId)
  pure row

-- | Compare the complete expected snapshot before replacing; stale writes abort.
saveOidcLogoutState :: S.OidcLogoutState -> S.OidcLogoutState -> Pg ()
saveOidcLogoutState expected replacement = do
  unless (S._logoutStateId expected == S._logoutStateId replacement) abortAuthTransaction
  current <- getOidcLogoutState (S._logoutStateId expected)
  unless (current == Just expected) abortAuthTransaction
  rows <- runUpdateReturningList $ save (S._oidcLogoutStates S.jizhangDb) replacement
  case rows of
    [_] -> pure ()
    _ -> abortAuthTransaction

-- | Require the bootstrap singleton; missing settings is a transaction conflict,
-- not a request to silently create a default provider configuration.
getAuthSettings :: Pg S.AuthSettings
getAuthSettings = do
  row <- runSelectReturningOne $ select $ do
    settings <- all_ (S._authSettings S.jizhangDb)
    guard_ (S._settingsId settings ==. val_ 1)
    pure settings
  maybe abortAuthTransaction pure row

-- | The revision is advanced here, never trusted from the replacement row.
saveAuthSettings :: Int64 -> S.AuthSettings -> Pg ()
saveAuthSettings revision replacement = do
  current <- getAuthSettings
  unless (S._settingsRevision current == revision && S._settingsId replacement == 1) abortAuthTransaction
  let next = replacement {S._settingsRevision = revision + 1}
  rows <- runUpdateReturningList $ save (S._authSettings S.jizhangDb) next
  unless (rows == [next]) abortAuthTransaction

getExternalIdentity :: Text -> Text -> Pg (Maybe S.ExternalIdentity)
getExternalIdentity issuer subject = runSelectReturningOne $ select $ do
  row <- all_ (S._externalIdentities S.jizhangDb)
  guard_ (S._identityIssuer row ==. val_ issuer &&. S._identitySubject row ==. val_ subject)
  pure row

getUserIdentity :: UUID -> Pg (Maybe S.ExternalIdentity)
getUserIdentity userId = runSelectReturningOne $ select $ do
  row <- all_ (S._externalIdentities S.jizhangDb)
  guard_ (S._identityUser row ==. val_ (S.UserId userId))
  pure row

insertExternalIdentity :: S.ExternalIdentity -> Pg ()
insertExternalIdentity row = runInsert $ insert (S._externalIdentities S.jizhangDb) (insertValues [row])

-- | Replace identity metadata only after a full snapshot match; issuer, subject
-- and owning account cannot change through this operation.
saveExternalIdentity :: S.ExternalIdentity -> S.ExternalIdentity -> Pg ()
saveExternalIdentity expected replacement = do
  unless
    ( S._identityIssuer expected == S._identityIssuer replacement
        && S._identitySubject expected == S._identitySubject replacement
        && S._identityUser expected == S._identityUser replacement
    )
    abortAuthTransaction
  current <- getExternalIdentity (S._identityIssuer expected) (S._identitySubject expected)
  unless (current == Just expected) abortAuthTransaction
  rows <- runUpdateReturningList $ save (S._externalIdentities S.jizhangDb) replacement
  case rows of
    [_] -> pure ()
    _ -> abortAuthTransaction

deleteExternalIdentity :: S.ExternalIdentity -> Pg ()
deleteExternalIdentity expected = do
  current <- getExternalIdentity (S._identityIssuer expected) (S._identitySubject expected)
  unless (current == Just expected) abortAuthTransaction
  rows <-
    runDeleteReturningList $
      delete
        (S._externalIdentities S.jizhangDb)
        ( \row ->
            S._identityIssuer row
              ==. val_ (S._identityIssuer expected)
              &&. S._identitySubject row
              ==. val_ (S._identitySubject expected)
        )
  unless (rows == [expected]) abortAuthTransaction

-- | Exact digest lookup only. The caller hashes the presented secret and must
-- separately check purpose, expiry, revocation and consumption before use.
getAccountInvitationByTokenHash :: ByteString -> Pg (Maybe S.AccountInvitation)
getAccountInvitationByTokenHash digest = runSelectReturningOne $ select $ do
  row <- all_ (S._accountInvitations S.jizhangDb)
  guard_ (S._invitationTokenHash row ==. val_ (digest))
  pure row

getOidcTransactionByStateHash :: ByteString -> Pg (Maybe S.OidcTransaction)
getOidcTransactionByStateHash digest = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcTransactions S.jizhangDb)
  guard_ (S._transactionStateHash row ==. val_ (digest))
  pure row

getOidcTransactionByHandoffHash :: ByteString -> Pg (Maybe S.OidcTransaction)
getOidcTransactionByHandoffHash digest = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcTransactions S.jizhangDb)
  guard_ (S._transactionHandoffHash row ==. val_ (Just digest))
  pure row

getOidcTransactionByContinuationHash :: ByteString -> Pg (Maybe S.OidcTransaction)
getOidcTransactionByContinuationHash digest = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcTransactions S.jizhangDb)
  guard_ (S._transactionContinuationHash row ==. val_ (Just digest))
  pure row

getOidcLogoutStateByHandoffHash :: ByteString -> Pg (Maybe S.OidcLogoutState)
getOidcLogoutStateByHandoffHash digest = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcLogoutStates S.jizhangDb)
  guard_ (S._logoutStateHandoffHash row ==. val_ (digest))
  pure row

getOidcLogoutStateByStateHash :: ByteString -> Pg (Maybe S.OidcLogoutState)
getOidcLogoutStateByStateHash digest = runSelectReturningOne $ select $ do
  row <- all_ (S._oidcLogoutStates S.jizhangDb)
  guard_ (S._logoutStateStateHash row ==. val_ (digest))
  pure row

-- | Cleanup never removes a live grant, even if it is already consumed.
cleanupExpiredAuth :: UTCTime -> Pg ()
cleanupExpiredAuth now = do
  runDelete $ delete (S._oidcTransactions S.jizhangDb) (\row -> S._transactionExpiresAt row <=. val_ now)
  -- An expired invitation can still be referenced by a live flow; keep it until that flow expires too.
  runDelete $
    delete
      (S._accountInvitations S.jizhangDb)
      ( \row ->
          S._invitationExpiresAt row
            <=. val_ now
            &&. not_
              ( exists_ $ do
                  transaction <- all_ (S._oidcTransactions S.jizhangDb)
                  guard_
                    ( S._transactionInvitation transaction
                        ==. just_ (primaryKey row)
                        &&. S._transactionExpiresAt transaction
                        >. val_ now
                    )
                  pure (S._transactionId transaction)
              )
      )
  runDelete $ delete (S._oidcLogoutStates S.jizhangDb) (\row -> S._logoutStateStateExpiresAt row <=. val_ now)
  runDelete $ delete (S._oidcLogoutEvents S.jizhangDb) (\row -> S._logoutEventExpiresAt row <=. val_ now)

-- | Timestamp currently unrevoked sessions without deleting their audit evidence.
-- This alone does not advance the account fence or fail pending login flows.
revokeUserSessions :: UUID -> UTCTime -> Pg ()
revokeUserSessions userId now =
  runUpdate $
    update
      (S._userSessions S.jizhangDb)
      (\row -> S._sessionRevokedAt row <-. val_ (Just now))
      (\row -> S._sessionUser row ==. val_ (S.UserId userId) &&. isNothing_ (S._sessionRevokedAt row))

revokeUserInvitations :: UUID -> UTCTime -> Pg ()
revokeUserInvitations userId now =
  runUpdate $
    update
      (S._accountInvitations S.jizhangDb)
      (\row -> S._invitationRevokedAt row <-. val_ (Just now))
      ( \row ->
          S._invitationUser row
            ==. val_ (S.UserId userId)
            &&. isNothing_ (S._invitationConsumedAt row)
            &&. isNothing_ (S._invitationRevokedAt row)
      )

failUserTransactions :: UUID -> Pg ()
failUserTransactions userId =
  runUpdate $
    update
      (S._oidcTransactions S.jizhangDb)
      (\row -> mconcat [S._transactionPhase row <-. val_ "failed", S._transactionResultPayloadEncrypted row <-. val_ Nothing])
      ( \row ->
          S._transactionUser row
            ==. val_ (S.UserId (Just userId))
            &&. S._transactionPhase row
            /=. val_ "consumed"
            &&. S._transactionPhase row
            /=. val_ "failed"
      )

failSessionTransactions :: UUID -> Pg ()
failSessionTransactions sessionId =
  runUpdate $
    update
      (S._oidcTransactions S.jizhangDb)
      (\row -> mconcat [S._transactionPhase row <-. val_ "failed", S._transactionResultPayloadEncrypted row <-. val_ Nothing])
      ( \row ->
          S._transactionSession row
            ==. val_ (S.UserSessionId (Just sessionId))
            &&. S._transactionPhase row
            /=. val_ "consumed"
            &&. S._transactionPhase row
            /=. val_ "failed"
      )

-- | Fence the account, revoke sessions and invitations, and fail unfinished flows.
-- These statements are atomic only within the caller's surrounding transaction.
revokeUserAuthentication :: UUID -> UTCTime -> Pg ()
revokeUserAuthentication userId now = do
  runUpdate $
    update
      (S._users S.jizhangDb)
      (\row -> S._authInvalidatedAt row <-. val_ (Just now))
      (\row -> S._userId row ==. val_ userId)
  revokeUserSessions userId now
  revokeUserInvitations userId now
  failUserTransactions userId

-- | Credential writes compare the full verified user snapshot, including NULL
-- password hashes and the invalidation fence. Serializable conflicts roll back.
saveUserAuthentication :: S.User -> S.User -> Pg ()
saveUserAuthentication expected replacement = do
  unless (S._userId expected == S._userId replacement) abortAuthTransaction
  current <- runSelectReturningOne $ select $ do
    row <- all_ (S._users S.jizhangDb)
    guard_ (S._userId row ==. val_ (S._userId expected))
    pure row
  unless (current == Just expected) abortAuthTransaction
  rows <- runUpdateReturningList $ save (S._users S.jizhangDb) replacement
  case rows of
    [_] -> pure ()
    _ -> abortAuthTransaction

-- | Include expired and revoked rows; callers decide which sessions to display
-- or accept rather than treating this listing as an authentication decision.
listUserSessions :: UUID -> Pg [S.UserSession]
listUserSessions userId = runSelectReturningList $ select $ do
  row <- all_ (S._userSessions S.jizhangDb)
  guard_ (S._sessionUser row ==. val_ (S.UserId userId))
  pure row

listPendingUserInvitations :: UUID -> UTCTime -> Pg [S.AccountInvitation]
listPendingUserInvitations userId now = runSelectReturningList $ select $ do
  row <- all_ (S._accountInvitations S.jizhangDb)
  guard_
    ( S._invitationUser row
        ==. val_ (S.UserId userId)
        &&. S._invitationExpiresAt row
        >. val_ now
        &&. isNothing_ (S._invitationConsumedAt row)
        &&. isNothing_ (S._invitationRevokedAt row)
    )
  pure row

-- | Consume an unexpired, unrevoked invitation against its expected snapshot.
-- Purpose and token proof must already have been verified by the caller.
consumeAccountInvitation :: S.AccountInvitation -> UTCTime -> Pg ()
consumeAccountInvitation expected now = do
  unless
    ( S._invitationExpiresAt expected > now
        && S._invitationConsumedAt expected == Nothing
        && S._invitationRevokedAt expected == Nothing
    )
    abortAuthTransaction
  -- Consume before revoking account grants, which would otherwise invalidate this expected snapshot.
  saveAccountInvitation expected (expected {S._invitationConsumedAt = Just now})

-- | Require the expected live phase, then save against the full snapshot.
-- The caller chooses a legal replacement phase and verifies browser/provider proofs.
transitionOidcTransaction :: S.OidcTransaction -> Text -> S.OidcTransaction -> UTCTime -> Pg ()
transitionOidcTransaction expected phase replacement now = do
  unless (S._transactionPhase expected == phase && S._transactionExpiresAt expected > now) abortAuthTransaction
  saveOidcTransaction expected replacement

-- | Consume only the short-lived handoff proof; the independent logout-return
-- state remains usable until its own expiry or consumption.
consumeLogoutHandoff :: S.OidcLogoutState -> UTCTime -> Pg ()
consumeLogoutHandoff expected now = do
  unless (S._logoutStateHandoffExpiresAt expected > now && S._logoutStateHandoffConsumedAt expected == Nothing) abortAuthTransaction
  saveOidcLogoutState expected (expected {S._logoutStateHandoffConsumedAt = Just now})

consumeLogoutState :: S.OidcLogoutState -> UTCTime -> Pg ()
consumeLogoutState expected now = do
  unless (S._logoutStateStateExpiresAt expected > now && S._logoutStateStateConsumedAt expected == Nothing) abortAuthTransaction
  saveOidcLogoutState expected (expected {S._logoutStateStateConsumedAt = Just now})

failInvitationTransactions :: UUID -> Pg ()
failInvitationTransactions invitationId =
  runUpdate $
    update
      (S._oidcTransactions S.jizhangDb)
      (\row -> mconcat [S._transactionPhase row <-. val_ "failed", S._transactionResultPayloadEncrypted row <-. val_ Nothing])
      ( \row ->
          S._transactionInvitation row
            ==. val_ (S.AccountInvitationId (Just invitationId))
            &&. S._transactionPhase row
            /=. val_ "consumed"
            &&. S._transactionPhase row
            /=. val_ "failed"
      )

-- | Revoke one expected session and discard unfinished flow payloads bound to it.
-- Keep both writes inside the same caller-owned Serializable transaction.
revokeSession :: S.UserSession -> UTCTime -> Pg ()
revokeSession expected now = do
  saveUserSession expected (expected {S._sessionRevokedAt = Just now})
  failSessionTransactions (S._sessionId expected)

-- | Replacement issuance leaves password-reset invitations of other purposes intact.
revokeInvitationsByPurpose :: UUID -> Text -> UTCTime -> Pg ()
revokeInvitationsByPurpose userId purpose now = do
  pending <- listPendingUserInvitations userId now
  mapM_
    ( \row ->
        if S._invitationPurpose row == purpose
          then do
            saveAccountInvitation row (row {S._invitationRevokedAt = Just now})
            failInvitationTransactions (S._invitationId row)
          else pure ()
    )
    pending

-- | Every provider revision change invalidates provider-bound grants; password
-- reset grants and local-password sessions are unaffected.
invalidateProviderGrants :: UTCTime -> Bool -> Pg ()
invalidateProviderGrants now revokeSessions = do
  runUpdate $
    update
      (S._oidcTransactions S.jizhangDb)
      (\row -> mconcat [S._transactionPhase row <-. val_ "failed", S._transactionResultPayloadEncrypted row <-. val_ Nothing])
      (\row -> S._transactionPhase row /=. val_ "consumed" &&. S._transactionPhase row /=. val_ "failed")
  runUpdate $
    update
      (S._accountInvitations S.jizhangDb)
      (\row -> S._invitationRevokedAt row <-. val_ (Just now))
      ( \row ->
          S._invitationPurpose row
            ==. val_ "oidc_link"
            &&. isNothing_ (S._invitationConsumedAt row)
            &&. isNothing_ (S._invitationRevokedAt row)
      )
  if revokeSessions
    then
      runUpdate $
        update
          (S._userSessions S.jizhangDb)
          (\row -> S._sessionRevokedAt row <-. val_ (Just now))
          (\row -> S._sessionMethod row ==. val_ "oidc" &&. isNothing_ (S._sessionRevokedAt row))
    else pure ()

-- | Fetch retained events for commit-time sid/sub matching. In particular this
-- fences an identity which was unknown while its token request was in flight.
getLogoutEventsSince :: Text -> UTCTime -> UTCTime -> Pg [S.OidcLogoutEvent]
getLogoutEventsSince issuer createdAt now = runSelectReturningList $ select $ do
  row <- all_ (S._oidcLogoutEvents S.jizhangDb)
  guard_
    ( S._logoutEventIssuer row
        ==. val_ issuer
        &&. S._logoutEventReceivedAt row
        >=. val_ createdAt
        &&. S._logoutEventExpiresAt row
        >. val_ now
    )
  pure row

-- | Candidate SSO sessions for a verified provider logout notification.
listOidcSessionsForIssuer :: Text -> Pg [S.UserSession]
listOidcSessionsForIssuer issuer = runSelectReturningList $ select $ do
  row <- all_ (S._userSessions S.jizhangDb)
  guard_
    ( S._sessionMethod row
        ==. val_ "oidc"
        &&. S._sessionOidcIssuer row
        ==. val_ (Just issuer)
        &&. isNothing_ (S._sessionRevokedAt row)
    )
  pure row

-- Payloads are encrypted; callers authenticate and match known proofs in memory.
-- In-flight unknown identities are fenced by the retained logout event instead.
listPendingOidcTransactions :: Pg [S.OidcTransaction]
listPendingOidcTransactions = runSelectReturningList $ select $ do
  row <- all_ (S._oidcTransactions S.jizhangDb)
  guard_ (S._transactionPhase row /=. val_ "consumed" &&. S._transactionPhase row /=. val_ "failed")
  pure row
