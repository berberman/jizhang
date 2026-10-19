# Authentication

## Identity

Local signup requires a unique username, display name, email and password. Handles are stored lowercase without `@`; local login accepts a handle with optional `@` or an email. Display names may be shared. SSO accounts can start without a local password.

Profile editing changes the handle/display name without changing the user UUID, ledger references or current session. Selection-backed mutations submit UUIDs, not mutable handles. Email and verification state are private; shared group/ledger responses expose only public identity. Self-service email editing and email verification are not implemented. See [Ledger](ledger.md) for CSV identity resolution.

## Session and failure behavior

Application bearer tokens identify a database session as well as a user. Every protected action requires an enabled account and a matching, unrevoked, unexpired session. Sessions last 24 hours without refresh; legacy UUID-only tokens are rejected. Expired one-use grants are removed at startup and during authentication operations; live grants are retained.

Bearer credentials and continuation/invitation tokens live only in memory, not localStorage, sessionStorage or browser cookies. Browser reload, including hash-route reload, requires sign-in. Sign-out and protected 401 responses clear protected state and unsaved drafts. Account deletion conflicts preserve the session and financial history. Ledger-specific draft and CSV failure behavior is documented in [Ledger](ledger.md).

With a stable `JWT_SECRET_KEY` and active, unexpired, unrevoked database session, a backend restart alone does not invalidate the token. Browser reload still loses the in-memory token regardless of backend key stability.

Application sign-out revokes only the current server session and its bound authorization attempts. The client clears its in-memory session immediately; network failure reports that server revocation could not be confirmed. A late logout response cannot clear a newer sign-in. Account deletion clears the client without a second request using the deleted credential.

Provider sign-out revokes the application session before contacting the provider. Its API response contains only a 60-second, one-use backend handoff; the backend alone redirects with the encrypted stored ID-token hint. The five-minute, one-use callback always returns to the configured login page. Canceling provider sign-out never restores the application session. A missing or unavailable provider logout endpoint is reported after local revocation.

Signed backchannel notifications match the configured issuer and `sid` (plus `sub` when supplied), or issuer plus `sub` without a `sid`. They preserve local-password and unrelated provider sessions, work while the matching provider is disabled, and retain idempotent logout events for at least 30 minutes to fence in-flight authorization and later registration/link decisions.

## Sign-in and credential changes

The OIDC callback only resolves a short-lived, encrypted provider proof. It does not create an account, link an identity or issue a session. The browser must redeem its one-use handoff with the original verifier before choosing registration or explicit linking. Matching emails are never proof of account ownership. Registration hints are editable and never overwrite an existing profile. Provider-backed accounts have no local password unless one is explicitly established.

Flutter web offers **Continue with provider** and exposes local and SSO signup independently. Failed sign-in, registration and explicit-link requests retain editable drafts. An unlinked provider identity opens **Complete sign-in** to review profile hints or choose **Link to existing account** and prove that account's current password.

SSO invitations show the intended target and require explicit identity confirmation after provider authentication. Denial does not consume the invitation. Password-reset invitations only inspect on opening, require a new password and confirmation, and never sign in automatically. Invitation tokens are removed from the router/browser URL while the form remains open; reloading the cleaned URL requires reopening the original one-time link.

The only sessionStorage authentication data is a bounded pending redirect proof: flow ID, verifier, safe return route, purpose and creation time. It belongs to the originating tab and is removed before callback exchange. Copied callbacks without that tab proof fail before exchange. Provider navigation is top-level, not a popup. Bearer API requests use noncredentialed CORS. Native provider redirects are unsupported; see [native targets](../client/README.md#native-targets).

Credential changes require the current local password or fresh, explicit provider reauthentication, depending on the operation. Ordinary SSO login does not grant that proof. In **Account**, sign-in methods expose password changes, provider verification, identity linking/unlinking and distinct local/provider sign-out. Current-password verification completes on the server before the new-password dialog opens. Establishing a first password or removing one requires **Verify with provider** (`prompt=login`, `max_age=1`), not ordinary SSO sign-in. The positive one-second age activates freshness checks in providers that treat zero as unset; authentication completed within the last second may be reused. The backend still validates the signed authentication time against the verification attempt and rejects missing, stale or future proof.

Password change/removal, unlink and invitation password reset revoke account sessions and outstanding grants. Successful credential changes clear the client session and require fresh sign-in. Reset does not enable a disabled account, remove its SSO identity or automatically sign in. Last-method guards prevent removing an enabled account's only viable sign-in method. Security changes retain unrelated failed-save profile drafts; external navigation and sign-out respect unsaved-profile confirmation.

Failed provider callbacks show the safe rejection category rather than treating every failure as cancellation. Account verification/linking failures return through Account; after a full provider redirect, the existing in-memory-session policy may require signing in again. Callback URLs and pending browser proof are still cleared before exchange.

## Configuration ownership

### Keys and runtime URLs

`JWT_SECRET_KEY` signs application and admin JWTs. It must be canonical standard base64 encoding of exactly 32 independent random bytes. Keep it stable across restarts; changing it invalidates signatures, not ledger rows. Standalone launches generate an ephemeral key if it is unset. An empty or invalid supplied key fails before database initialization.

`OIDC_SECRET_KEY` encrypts persisted authentication data and must independently contain canonical standard base64 of exactly 32 random bytes. Never reuse the JWT key. Keep it stable and back it up securely with the database: replacement makes saved authentication ciphertext unreadable. Neither key belongs in frontend configuration.

OIDC and one-time sign-in invitations additionally require `PUBLIC_API_URL` and `FRONTEND_URL` (ending in `/`). Both must be absolute URLs without credentials, query or fragment. HTTPS is required except for literal loopback development hosts. Base-path prefixes are retained in callback URLs. The frontend origin must equal the API origin or appear explicitly in `CLIENT_ORIGINS`. Missing values leave local/operator authentication available; malformed explicit values fail startup.

The local launcher supplies computed URLs and creates/reuses an owner-only `.local/jizhang/oidc-secret.key` when the encryption key is unset. It rejects malformed existing keys rather than silently replacing them. Never publish that file. See [Development](development.md) for launcher details, [Flutter API configuration](../client/README.md#api-configuration) for client URL precedence, and [Deployment](deployment.md) for production routing and protected runtime configuration.

Authentication storage is a fresh-schema cutover, not an in-place migration. Deploy against a separately provisioned database initialized with the packaged schema; do not reset an existing ledger or persistent launcher workspace.

### Database or environment authority

Authentication settings default to database ownership. To manage the complete provider and signup policy at every backend startup, set `AUTH_CONFIG_SOURCE=environment` and explicitly set all three booleans to `true` or `false`:

- `OIDC_ENABLED`
- `LOCAL_REGISTRATION_ENABLED`
- `SSO_REGISTRATION_ENABLED`

Provider fields are `OIDC_PROVIDER_NAME` (default `Authentik`), `OIDC_ISSUER`, `OIDC_CLIENT_ID` and `OIDC_CLIENT_SECRET`. Empty nullable values clear saved values. Enabled SSO requires all credentials, valid runtime URLs and the stable encryption key. Disabling a linked provider must retain its issuer/client ID until identities are explicitly unlinked. Provider secrets stay backend-only and encrypted at rest.

Environment ownership makes operator authentication settings read-only, including unsaved connection testing. Identical restarts preserve revision, ciphertext and live grants. Actual changes invalidate provider-bound grants; disabling revokes SSO sessions without disabling password sign-in. Failed startup reconciliation serves no application.

All instances sharing a database must use the same configuration. Start with one backend replica and recreate it for changes rather than rolling through mixed configurations. To return authority to the database, set `AUTH_CONFIG_SOURCE=database` and remove or empty all seven provider/policy variables listed above. The last committed settings remain, and admin editing returns. Nonempty managed values in database mode fail rather than being ignored. Hand-back does not restore older defaults.

## Operator management

Open `/admin-ui` on the API origin and sign in with an operator account, not an application account. Application bearer tokens never authorize operator APIs. Empty admin bootstrap values disable bootstrap; a nonempty username/password must be supplied together. Bootstrap only inserts a missing admin and never resets an existing password.

Operator cookie and bearer credentials expire 24 hours after login. Authenticated
requests do not renew that deadline. Legacy operator tokens without an expiration
are rejected. Operator logout clears browser cookies only after authenticated
JWT access or cookie authentication with matching XSRF proof; unauthenticated
retries succeed without changing cookies. Unlike application sessions, operator
JWTs are stateless: logout does not revoke a separately retained token before its
expiry. The operator document cannot be embedded in a frame.

The **Authentication** page controls local/SSO public registration separately from provider enablement. Local sign-in remains available when SSO is disabled. Under database ownership, **Edit authentication** uses a private revision-bound draft. A blank client-secret field retains the saved secret; saved secrets are never returned or prefilled. **Test unsaved connection** checks discovery/JWKS without saving and does not prove token-endpoint acceptance of the client secret. A stale save returns a conflict and retains the draft; reload settings before applying it.

Disabled providers may retain partial configuration without discovery access. Disabling SSO revokes its application sessions, not local-password sessions. Every settings revision invalidates pending OIDC flows and SSO invitations; password-reset invitations are independent. Unlink identities before changing issuer/client ID or clearing the provider. Review the SSO-only-user count before disabling a provider those users depend on.

The **Users** page provisions local-password or pending-SSO users independently of public signup switches. Details expose profile/status, sign-in methods and pending invitations, never credential hashes or grant tokens. Changing email clears verification status. Disabling an account revokes sessions and outstanding grants; re-enabling restores neither. **Revoke all sessions** permits a later fresh sign-in. Removing a password or identity revokes sessions/grants and cannot remove an enabled account's last viable method; a deliberately disabled account may have neither.

Generate password-reset or SSO-linking invitations explicitly and share the one-time URL out of band. The console reveals it only in the issuance dialog; closing, navigating or ending the operator session clears its URL DOM content. Issuance/inspection changes no credentials and revokes no sessions. Replacement or explicit revocation invalidates the previous unused invitation. SSO linking requires real provider authentication and explicit target-account confirmation, regardless of matching email. Reset consumption is one-use, revokes sessions/grants, preserves identity/status and never signs in automatically. No SMTP service or public email-reset endpoint is required.

## Provider setup: Authentik

Create a confidential OAuth2/OIDC provider with authorization-code flow and PKCE. Use the default per-application issuer mode, scopes `openid profile email`, an asymmetric **RS256** signing key and no encryption key. Configure strict URLs using the public API base, including any deployment prefix:

| Setting | URL |
| --- | --- |
| Redirect URI | `PUBLIC_API_URL/auth/oidc/callback` |
| Back-channel logout method and URL | `PUBLIC_API_URL/auth/oidc/backchannel-logout` |
| Post-logout redirect URI | `PUBLIC_API_URL/auth/oidc/logout/callback` |

Under database ownership, copy the returned issuer, client ID and client secret into the operator Authentication editor, test discovery, then save and enable. Under environment ownership, supply the corresponding managed fields described above. Code exchange uses HTTP Basic client authentication plus S256 PKCE. SSO signup does not require verified email and never automatically links an existing account.

Provider-wide/global logout also depends on Authentik's logout-flow policy; the application button cannot override it. Validate production login, explicit linking, reauthentication and both logout directions in your deployment before rollout.
