# Advanced configuration

Start with the short [`.env.example`](../.env.example) and the
[deployment guide](deployment.md). Most installations do not need these overrides.
Keep independent persistent signing/encryption keys, secure cookies, and request
safeguards; reducing the example file does not disable them.

Compose reads overrides from its `--env-file` or the invoking shell. An unset
backend policy key uses the backend's default. A supplied value, including an
invalid empty numeric value, reaches backend validation. Do not print expanded
Compose configuration containing secrets; use `config --quiet`.

## Addresses and topology

The recommended deployment exposes one HTTPS origin through an operator-managed
reverse proxy. Compose still runs separate API and static-web services and does
not include a proxy. Route API paths to the backend as described in
[ingress routing](deployment.md#kubernetes-traefik-and-cnpg).

| Setting | Purpose and default |
| --- | --- |
| `PUBLIC_API_URL` | Required by Compose; browser-visible absolute API base used for authentication callbacks and invitations. No credentials, query, or fragment. |
| `FRONTEND_URL` | Required by Compose; absolute browser-visible frontend base ending in `/`. |
| `API_BASE_URL` | Web runtime choice: `same-origin` by default, or an absolute browser-visible HTTP(S) API URL, optionally with a path prefix. Empty is invalid. |
| `CLIENT_ORIGINS` | Backend CORS allowlist for separate frontend origins; comma-separated exact origins, no trailing slash or wildcard. Unset means no cross-origin access. |
| `ADMIN_COOKIE_SECURE` | Defaults to `true`, including behind TLS termination. Only literal-loopback HTTP testing should set `false`. |
| `API_BIND_HOST`, `WEB_BIND_HOST` | Compose host publications; both default to `127.0.0.1`. These are not browser-visible URLs. |
| `API_PORT`, `WEB_PORT` | Compose host ports; default to 8080 and 3000. Both container services listen on 8080. |
| `BIND_HOST` | Direct backend launches only; unset retains Warp's default. The local launcher chooses loopback; the image chooses `*6`. |
| `JIZHANG_BACKEND_IMAGE`, `JIZHANG_WEB_IMAGE` | Compose image references; default to `jizhang-backend:local` and `jizhang-web:local`. |

For separate frontend and API origins, set all addresses explicitly, for example:

```dotenv
PUBLIC_API_URL=https://api.example.test
FRONTEND_URL=https://ledger.example.test/
API_BASE_URL=https://api.example.test
CLIENT_ORIGINS=https://ledger.example.test
```

`API_BASE_URL` no longer inherits `PUBLIC_API_URL` when omitted. Its default is
always `same-origin`. Replace old empty `API_BASE_URL` values with `same-origin`.
The runtime file must likewise contain that token or an API URL, not empty text.
A missing, empty, or malformed runtime file stops frontend startup.

For local HTTP testing without the launcher, the corresponding explicit overrides
are the loopback API/frontend URLs, the frontend's exact `CLIENT_ORIGINS`, an
explicit API URL in `API_BASE_URL`, and `ADMIN_COOKIE_SECURE=false`. Prefer
[`scripts/run-local.py`](../scripts/run-local.py), which supplies local settings
without changing production defaults.

The compile-time Dart `API_BASE_URL` override remains available for development
and specialized static builds. It takes precedence over the runtime file and
requires rebuilding to change. See [client configuration](../client/README.md#api-configuration).
Neither frontend configuration mechanism may contain credentials or backend-only
secrets.

## Existing PostgreSQL and connection options

Bundled PostgreSQL uses a dedicated application role and a separate superuser
password. With an existing database, omit the PostgreSQL Compose override and
configure its connection instead. Never connect the application as a superuser.

| Setting | Behavior |
| --- | --- |
| `DATABASE_URL` | Optional libpq keyword DSN or PostgreSQL URI. Nonempty values override matching `PG*` fields, not unspecified fields. Unset/empty delegates to `PG*`. |
| `PGHOST`, `PGPORT` | Compose defaults to `postgres:5432`; replace the host for an existing database. |
| `PGDATABASE`, `PGUSER` | Compose defaults to database and application role `jizhang`. |
| `PGPASSWORD` | Application-role password; also used by the bundled database's first-volume initialization. |
| `POSTGRES_PASSWORD` | Separate bundled-database superuser password; never supplied to the backend. Not needed with an existing database. |
| `PGSSLMODE` | Compose defaults to `prefer` for the bundled network; use `verify-full` for a remote database. |
| `PGSSLROOTCERT` | Compose defaults to `/etc/ssl/certs/ca-certificates.crt`; mount a private CA read-only and use its container path when needed. |
| `PGCONNECT_TIMEOUT` | Image/Compose default is 10 seconds. The backend connection manager additionally caps each establishment attempt at five seconds. |

Pass passwords as data, not by concatenating them into a DSN. Bootstrap runs only
on an empty PostgreSQL volume; editing `.env` does not rotate existing database
passwords or migrate data. Preserve existing volumes. See
[database initialization](deployment.md#postgresql-initialization-and-persistence).

## Environment-owned authentication

By default, configure providers and signup policy in the admin console. No
`AUTH_CONFIG_SOURCE` or provider variables are needed for that mode.

Automated deployments can make the environment authoritative at every backend
startup. Set `AUTH_CONFIG_SOURCE=environment` and supply all three policy booleans
as exactly `true` or `false`:

```dotenv
AUTH_CONFIG_SOURCE=environment
OIDC_ENABLED=true
LOCAL_REGISTRATION_ENABLED=false
SSO_REGISTRATION_ENABLED=false
OIDC_PROVIDER_NAME=Authentik
OIDC_ISSUER=https://id.example.test/application/o/jizhang/
OIDC_CLIENT_ID=synthetic-jizhang-client
OIDC_CLIENT_SECRET=
```

Fill the provider-issued client secret privately before enabling OIDC. This is
not `OIDC_SECRET_KEY`: that separate persistent key encrypts authentication data.
Enabled OIDC requires the configured URLs, provider credentials, valid discovery,
and the stable encryption key.

`OIDC_PROVIDER_NAME` defaults to `Authentik` when absent/empty; an explicit label
must contain 1..100 non-control characters. Empty issuer/client/secret clears the
saved value. Preserve issuer paths/trailing slashes and nonblank client values
exactly. A disabled provider can retain partial settings, but identities already
linked to it require its issuer/client ID until explicitly unlinked. Signup flags
control registration, not whether existing accounts may sign in.

Environment ownership makes admin authentication settings read-only. Unchanged
restarts preserve settings revision and live grants. Actual changes invalidate
provider-bound grants; disabling SSO revokes SSO sessions without disabling
password sign-in. Failed reconciliation prevents application startup. Use one
backend replica and recreate it for authoritative changes rather than rolling
through mixed settings.

To return control to the admin console, remove `AUTH_CONFIG_SOURCE` or set it to
`database`, and remove/empty all seven managed provider/policy variables. The last
committed settings remain; hand-back does not restore defaults. Database mode
rejects nonempty managed values even while OIDC is disabled. See
[authentication operations](authentication.md#operator-management) for editing,
provider testing, and account recovery.

## Receipt image extraction

Receipt extraction is disabled by default. Administrators control its enabled
state independently from the effective credential. The model is fixed to
`gpt-5.4-mini-2026-03-17` with low reasoning; no custom endpoint, automatic model
fallback, or automatic retry is supported.

| Setting | Behavior |
| --- | --- |
| `OPENAI_API_KEY` | Optional server-only credential override. Empty counts absent; nonempty values must have no whitespace/control characters and be at most 512 characters. Overrides, but never deletes, an encrypted database credential. |
| `RECEIPT_AI_SECRET_KEY` | Optional independent canonical base64 32-byte key for encrypted administrator-managed credentials. Keep it stable, backed up, and distinct from JWT/OIDC keys. An explicitly empty or malformed value fails startup. |

The encryption key is required before storing a credential through the admin
API, not for environment-key operation, manual receipts, or reading usage.
Changing it does not re-encrypt existing credentials. Without the matching key,
a retained database credential is unavailable. Removing an environment override
exposes the retained database configuration again.

Compose forwards these variables only to the backend. Never put them in Flutter
defines, web runtime configuration, URLs, browser storage, or source control. The
local launcher inherits operator environment variables and does not generate an
ephemeral receipt encryption key.

The effective decoded image cap is the smaller of 5 MiB and three quarters of
the bulk body cap after reserving 256 KiB for JSON/records/review. A zero result
disables image import without disabling manual entry. JPEGs also have independent
8192px dimension and 16-million-pixel limits. The entire final save remains
subject to `REQUEST_BULK_BODY_MAX_BYTES`.

Selecting an image is not an API call to OpenAI. Only explicit extraction sends
it to OpenAI; `store=false` does not replace the provider's retention terms.
Canceled or failed requests may still be charged. If recording an attempt fails,
check usage before explicitly retrying. Usage estimates cover only calls made by
this installation, not an OpenAI invoice or account-wide activity.

Admin settings are available at `GET/PUT /admin/receipt-ai/settings`. Responses
report credential presence/source only, never plaintext, ciphertext, or a masked
suffix. Updates require the current revision and an explicit keep/replace/clear
action. An environment-owned credential is read-only; keeping it can still
enable or disable extraction. Local availability means configuration is usable,
not that the provider has accepted the credential.

`GET /admin/receipt-ai/usage` reports calls, all UTC daily/model buckets, and a
server-paginated call list. `from` and `to` are inclusive UTC days; either may be
omitted. Group/status filters apply before aggregates and pagination. Reported
tokens exclude unknown usage. A missing or invalid counter remains unknown, and
unknown-cost calls are visibly unpriced rather than assumed free.

The initial immutable price snapshot is USD 0.75 per million input tokens,
USD 0.075 per million cached input tokens, and USD 4.50 per million output tokens.
Reasoning tokens are already part of output. Estimates use integer nano-USD
arithmetic with nine decimal places; future prices do not rewrite old calls.


The admin console's **Receipt AI** page provides settings and usage separately
from financial reports. **Edit settings** opens a guarded draft with a masked
**OpenAI API key** field; leaving it blank keeps the credential. **Clear saved
key** stages removal after confirmation and takes effect only with **Save
settings**, alongside the draft enabled value. Failed saves retain the draft;
a revision conflict requires reloading current settings before another save.
Closing the editor removes the replacement input. Environment-managed keys
cannot be replaced or cleared here.

The **Period** selector covers the last 7, 30, or 90 inclusive UTC days, all time,
or custom dates. The default is 30 days. Presets retain group/status restrictions;
the applied summary keeps those restrictions visible when **Filters** is closed.
Invalid drafts leave previous results labeled as previous; pagination uses
applied filters, never a partially edited draft. **Usage details** includes every
matching daily/model bucket and all reported token categories. Per-call
**Details** exposes tokens and provider time. Labeled local scrolling keeps
wide tables within the page without discarding columns.



## Request and proxy overrides

Protection is always enabled, including direct backend launches. Defaults live in
`Jizhang.RequestProtection.defaultRequestProtectionConfig`; the backend parser
uses that record, not the basic template or Compose. Settings apply at backend
startup; invalid values fail before connecting to PostgreSQL. Numeric values must
be nonempty ASCII decimal integers.

| Environment key | Default | Range and burst |
| --- | --- | --- |
| `RATE_LIMIT_IP_PER_MINUTE` | 600 | 1..1,000,000; burst `min(value,120)` |
| `RATE_LIMIT_AUTH_PER_IP_PER_MINUTE` | 20 | 1..1,000,000; burst `min(value,10)` |
| `RATE_LIMIT_ACCOUNT_PER_MINUTE` | 300 | 1..1,000,000; burst `min(value,60)` |
| `RATE_LIMIT_EXPENSIVE_PER_ACCOUNT_PER_MINUTE` | 30 | 1..1,000,000; burst `min(value,10)` |
| `RATE_LIMIT_CREDENTIAL_PER_MINUTE` | 10 | 1..1,000,000; burst `min(value,5)` |
| `REQUEST_AUTH_BODY_MAX_BYTES` | 65536 (64 KiB) | 1..1,048,576 |
| `REQUEST_BODY_MAX_BYTES` | 1048576 (1 MiB) | 1..16,777,216 |
| `REQUEST_BULK_BODY_MAX_BYTES` | 8388608 (8 MiB) | 1..67,108,864 |
| `REQUEST_MAX_IN_FLIGHT` | 32 | 1..1024 |
| `REQUEST_MAX_EXPENSIVE_IN_FLIGHT` | 4 | 1..64; no greater than total in-flight |
| `RATE_LIMIT_MAX_KEYS` | 10000 | 1..1,000,000 combined live keys |
| `TRUSTED_PROXY_CIDRS` | unset | At most 32 comma-separated IPv4/IPv6 CIDRs |

Require auth body cap <= ordinary cap <= bulk cap. Unset or whitespace-only proxy
configuration trusts no forwarding metadata, including from loopback. Each
nonempty CIDR needs a numeric address and an explicit valid prefix; empty
comma-separated elements fail validation. Trust only actual ingress peers and
verified proxy-chain CIDRs. Proxy trust does not block direct access: also restrict
backend exposure using bindings, a firewall, or network policy.

Tune only when your instance's supported imports and workload require it. Higher
body caps and concurrency can increase buffered memory. Limits are per backend
process, reset on restart, and are not distributed quotas or DDoS protection.
See [request behavior](deployment.md#backend-request-safeguards) and
[ingress safeguards](deployment.md#public-ingress-safeguards).
