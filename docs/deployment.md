# Deployment

Commands run from the repository root unless noted. For local launcher setup, see [Development](development.md). Authentication keys, provider policy and startup reconciliation are defined in [Authentication](authentication.md#configuration-ownership).

## Application images

Nix `dockerTools` produces independent `backendImage` and `webImage` archives for `x86_64-linux` (Docker `linux/amd64`) and `aarch64-linux` (`linux/arm64`). These Docker-loadable `.tar.gz` files are neither build contexts nor multiarchitecture indexes.

The backend image includes installed SQL, admin UI, currency catalog, attribution and its exact Nix runtime closure. It contains neither a checkout nor PostgreSQL. The web image serves Flutter assets only: no Nginx, API proxy, embedded PostgreSQL or Compose DNS dependency. Both services listen on HTTP port 8080 and run nonroot. PostgreSQL, TLS and ingress are external infrastructure. Image assembly needs no Docker daemon and does not install Nix in a container or provide an alternative source-build path.

Application image listeners support IPv6 and IPv4 on dual-stack hosts: backend
uses Warp's IPv6-preferred `BIND_HOST=*6`, and web binds `[::]:8080`, accepting
IPv4-mapped connections where the host permits dual-stack sockets. Keep IPv6
enabled for the web container and verify both families on the target nodes.
IPv6-first dual-stack Kubernetes Services can use `ipFamilyPolicy: PreferDualStack`
and `ipFamilies: [IPv6, IPv4]`; Service addresses do not enable a missing listener.
Compose keeps its loopback-only host port publications unchanged.

### Build archives safely

On a matching native Linux Nix host:

```sh
python3 scripts/build-images.py --system x86_64-linux --target all
```

Select `--system aarch64-linux` for a native arm64 Linux builder. `--system` is required; the helper does not infer Linux architecture from macOS. Targets `backend` and `web` build independently; `all` builds backend then web and stops on failure. Image-only Haskell packaging skips destructive integration tests; normal Nix packages and development shells retain their test behavior.

Flutter builds release assets with `/` base href and no compiled API URL, using the pinned SDK and sandboxed, hash-checked dependencies from `client/pubspec.lock`. It does not consume `client/build/web` or a host pub cache. Runtime images contain neither Flutter/Dart nor Nix build tools.

Before either Nix store imports source, the helper creates an owner-only temporary public-source snapshot. It copies only required flake/Cabal/license/changelog inputs, backend source/data, `nix`/`docker` packaging, and client `lib`, `web`, pubspec and lockfile. It excludes hidden entries, env files, caches, build outputs and arbitrary root files. Selected symlinks/special files or missing inputs fail before Nix. Untracked public implementation files are included without Git staging.

Do not bypass this boundary with raw `path:.` against an unfiltered working tree. Filtering after import cannot protect private files already copied into a store. A clean tracked public checkout may instead use `nix build .#packages.x86_64-linux.backendImage` and the corresponding `webImage`.

Each target invokes `nix build` with lock updates/writes disabled, `--max-jobs 1 --cores 1`, `--no-link --print-out-paths`, and matching explicit `--store` and `--eval-store`. Use `--store auto` on a matching Linux host. Output contains target-labelled archive paths, not private store URIs. Each Nix child has a private mode-0600 diagnostic log; success removes it and failure retains it. Sanitize failed logs before sharing. Interrupt/termination reaches only the helper-owned Nix process group, which is waited for before snapshot removal. The helper does not load/push images, retry builds, alter host configuration or remove unrelated files.

### Remote native Linux builders from macOS

Apple Silicon macOS cannot execute Linux derivations itself. Use a native Linux executor matching the target architecture; Docker's Linux VM is not automatically a Nix builder. Keep a modern local Nix evaluator and use the helper's matching explicit store/evaluation-store settings. The remote store must support import-from-derivation and sandboxed builds with the pinned dependencies.

Supply `BUILDER_STORE` through your existing SSH login/identity selection. Verify the host key through a trusted channel; never disable host-key checking or commit private connection settings. A synthetic URI is:

```text
ssh-ng://builder.example.test?remote-program=REMOTE_DAEMON&base64-ssh-public-host-key=ENCODED_KEY
```

`REMOTE_DAEMON` is the compatible absolute `nix-daemon` executable path. Percent-encode URI parameter values, using `%20` rather than `+` for spaces. `ENCODED_KEY` is standard base64 of `ssh-ed25519 BASE64_KEY`, not a known-hosts hostname line. Add login or an explicit `ssh-key` parameter only when needed; never embed passwords. Local job/core flags do not guarantee remote daemon limits or memory usage.

From the Mac:

```sh
python3 scripts/build-images.py --system x86_64-linux --target all --store "$BUILDER_STORE"
```

Returned paths belong to the remote store, not the Mac. Load archives on the matching-architecture remote Docker engine over the verified SSH connection. If needed locally, copy an archive through that connection into an owner-only file. Deploy the public Compose files and executable `docker/postgres-init.sh` together on the runtime host, where the bind mount resolves. Create a protected runtime env file there rather than copying a private checkout or developer `.env`.

### Load images

On the matching runtime engine, substitute the returned archive paths:

```sh
docker load --input /nix/store/REPLACE-backend-image.tar.gz
docker load --input /nix/store/REPLACE-web-image.tar.gz
```

The loaded references are `jizhang-backend:local` and `jizhang-web:local`. Set `JIZHANG_BACKEND_IMAGE`/`JIZHANG_WEB_IMAGE` if using your own preloaded or published references. Compose consumes images only: missing images fail instead of triggering a source build. The repository does not publish registry images automatically.

## Protected configuration and Compose

Copy [`.env.example`](../.env.example) under `umask 077`, keep `.env` mode 0600 and fill required blank values privately. Its comments show separate `openssl rand -base64 32` invocations writing JWT/OIDC keys directly to the protected file, plus independently generated application/superuser database passwords. Never display secrets, commit real env files or run expanded `compose config`; use `config --quiet`. Runtime environment secrets are visible to authorized Docker-engine administrators, not image layers or build arguments.

Compose env files are not shell scripts. Single-quote values containing `$`, `#`, whitespace or special characters; escape an embedded single quote as `\'`. Unquoted/double-quoted values may interpolate `$`. Pass `PGPASSWORD` as data rather than assembling a credential-bearing DSN/URI. An explicit `DATABASE_URL` may be a libpq keyword string or PostgreSQL URI; its specified fields override PG environment defaults.

The complete stack requires both files:

```sh
docker compose --env-file .env -f compose.yaml -f compose.postgres.yaml config --quiet
docker compose --env-file .env -f compose.yaml -f compose.postgres.yaml up -d --no-build --wait
```

For existing PostgreSQL, set `PGHOST`/credentials or `DATABASE_URL` and use only the base file:

```sh
docker compose --env-file .env -f compose.yaml config --quiet
docker compose --env-file .env -f compose.yaml up -d --no-build --wait
```

A Compose v2-compatible CLI is required; `docker-compose` may replace `docker compose` if installed. The two-file stack contains backend/web/postgres; the base stack contains backend/web with no bundled volume or database dependency. Neither includes a proxy. The ordinary project network allows outbound HTTPS for OIDC and exchange-rate providers. Web startup and health are independent of backend health.

Compose requires nonempty `PUBLIC_API_URL`, trailing-slash `FRONTEND_URL`, `JWT_SECRET_KEY` and `OIDC_SECRET_KEY`; backend URL/key validation still applies. See [configuration ownership](authentication.md#configuration-ownership) for key requirements and authentication policy changes.

### Origins, TLS and public access

For split origins, `CLIENT_ORIGINS` must list exact frontend origins without a trailing slash. There is no wildcard or credentialed bearer CORS, and no admin CORS. Default publications are API `127.0.0.1:8080` and web `127.0.0.1:3000`, adjustable through `API_BIND_HOST`/`API_PORT` and `WEB_BIND_HOST`/`WEB_PORT`. Admin lives at `PUBLIC_API_URL/admin-ui`, not the frontend origin.

Public exposure, perimeter limits, ingress policy and TLS are operator responsibilities; the backend also enforces the request safeguards below. Keep secure admin cookies enabled for TLS. The example's `ADMIN_COOKIE_SECURE=false` is only for loopback HTTP. Serve production frontend and API over HTTPS to avoid mixed-content blocking.

### PostgreSQL initialization and persistence

The bundled database has no host port and retains a project-scoped `postgres-data` volume. Its independent `POSTGRES_PASSWORD` is required only by the override and reaches only PostgreSQL; `PGPASSWORD` is the separate application password.

Initialization creates the application login as NOSUPERUSER/NOCREATEDB/NOCREATEROLE/NOREPLICATION and its owned database using quoted psql identifiers/literals, with password-bearing statement diagnostics disabled. It rejects reserved bootstrap names. TCP readiness avoids the entrypoint's temporary initialization server.

Bootstrap runs only on an empty volume. Changing env values does not rotate passwords or reset/migrate existing data. Never use `down -v` on data you intend to retain. Existing-DB mode requires a dedicated empty database or compatible Jizhang schema, owned/creatable by the application role. Startup retains `createTables` behavior; no legacy import, reset or general migration is supplied.

For remote PostgreSQL, use `PGSSLMODE=verify-full`. Public CA roots are included; mount a private CA read-only and set `PGSSLROOTCERT` to its container path. An empty `DATABASE_URL` delegates to PG variables, including the default 10-second `PGCONNECT_TIMEOUT`. Neither image bakes in a database hostname.

## Runtime-configured web container

The web server runs as UID/GID 1000 on port 8080, serves read-only assets and never forwards API traffic. `/auth/options`, `/admin-ui` and unknown assets return 404 rather than Flutter index fallbacks. Open admin on the API origin.

Supply only a public, browser-visible HTTP(S) `API_BASE_URL`, never Docker-internal names such as `http://backend:8080`, credentials or provider secrets. Empty/unset in the bare image means same-origin. Compose substitutes `PUBLIC_API_URL` only when `API_BASE_URL` is unset; explicitly empty selects same-origin behind an external gateway.

Before listening, the entrypoint validates the value, atomically writes `/public/runtime/api-base-url.txt` and execs the server. Invalid input or missing/unwritable runtime storage aborts startup. Only `/public/runtime` is writable: Compose supplies a UID/GID 1000 tmpfs; pods need an emptyDir with fsGroup 1000. No root startup, writable asset directory or asset copying is needed.

The runtime file contains only the public URL, never keys, provider credentials, cookies or executable JavaScript. The server sends `Cache-Control: no-store` for it and `no-cache` for other assets. Flutter fetches with a fresh URL cache key on every startup and fails closed on configuration errors. A nonempty manual Dart define still wins; see [Flutter API configuration](../client/README.md#api-configuration).

To change the API origin, recreate web with the new runtime value using the same image, then reload the page. API errors after successful configuration retain the application's retry/error UI. A stopped API does not make the independent web server unhealthy.

## Static web deployment

Manual compile-time builds are separate from the runtime-configured image workflow. From the repository root:

```sh
cd client
flutter build web --dart-define=API_BASE_URL=https://api.example.org
```

Deploy `client/build/web` contents to a static HTTP(S) server. Set the backend's exact `CLIENT_ORIGINS` and use HTTPS for both services in production. For a subdirectory mount, supply Flutter's `--base-href=/your-path/`. Hash routes such as `/#/groups/<group-id>/records` need no API-style history fallback. Never route `/auth`, `/users` or `/groups` API requests to the static index behind a shared proxy.

For a local release smoke, build with the local API URL, stop the development server, then run from the repository root:

```sh
python3 -m http.server 3000 --bind 127.0.0.1 --directory client/build/web
```

## Backend request safeguards

Protection is enabled for direct launches and behind ingress, with no disable switch. Settings apply at process startup; invalid values fail before connecting to PostgreSQL. All numeric values must be nonempty ASCII decimal integers in the following ranges. Absent settings use these defaults:

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
| `TRUSTED_PROXY_CIDRS` | empty | At most 32 comma-separated IPv4/IPv6 CIDRs |

Require auth body cap <= ordinary cap <= bulk cap. Unset or whitespace-only proxy configuration trusts no forwarding metadata. Each nonempty CIDR needs a numeric address and explicit valid prefix; empty comma-separated elements fail startup. Compose passes these settings only to backend and preserves invalid empty numeric overrides for backend validation.

The general IP bucket applies to every request except the liveness exemption below. Authentication IP/body classification covers `/auth/**` except exact `GET /auth/options`, both operator `POST /admin/auth/login` and `POST /admin/auth/session/login`, and `POST /users/me/reauthenticate`, `PUT /users/me/password`, `POST /users/me/oidc/start`. These requests also use the expensive admission pool.

Bulk body capacity applies only to `POST /groups/<gid>/import`, `POST /groups/<gid>/receipts`, `PUT /groups/<gid>/receipts/<rid>` and their `/admin/groups/...` equivalents. Bulk delete is not receipt creation. All other request bodies, including unknown paths and non-JSON content, use the ordinary cap. The backend checks declared length and buffers bounded incoming chunks before routing, so chunked uploads cannot mutate the ledger before an oversized body is rejected. Exactly-at-cap bodies proceed to normal validation.

The expensive pool also covers group reports/statistics, exchange-quote creation, operator user/admin provisioning, and operator authentication-settings PUT/test. Authenticated expensive endpoints share one UUID budget per actor across groups, routes and token rotations. The ordinary account budget is separate. Users and operators have independent namespaces even for identical UUIDs. Known username/email login aliases, OIDC password linking and account reauthentication/password change share the user's credential budget; both operator login paths share the operator's. Unknown valid identifiers use digests, not stored plaintext identifiers.

Buckets start full and refill continuously with a monotonic clock. Every admitted attempt counts, successful or not; later errors do not refund credits, and denials do not extend the refill delay or persist account lockouts. Rate exhaustion returns 429 and a positive integer `Retry-After` in seconds, rounded up until the next token. Full admission pools return 503 immediately without a waiting queue, with `Retry-After: 1`. The bounded key table also returns that 503 for unseen keys when live buckets fill it; existing keys keep working, and fully refilled buckets expire. Oversized bodies return 413, and malformed forwarding from trusted peers returns 400. These plain-text failures include no-store; sensitive routes retain no-referrer. Allowed user origins can read Retry-After through CORS; admin gains no CORS.

Slots cover body reading and response delivery, and release on errors, disconnects and cancellation. Warp has a 30-second connection inactivity timeout, not an asynchronous financial-handler deadline or a total body-read deadline. Operators must enforce the latter at ingress.

Counters are in memory for one backend process, reset on restart, and are not shared across replicas. Tune limits against actual login bursts, shared-IP/NAT traffic, report cost and upload sizes; these defaults are starting policies, not measured capacity. Increasing body caps and concurrency increases possible buffered memory. Counts provide neither a distributed quota nor DDoS protection; arrange shared external enforcement before scaling replicas.

Within one backend process, database actions share a single-slot connection manager
and remain serialized. CSV imports and receipt writes hold the slot for the
complete transaction, so other users' session checks and ledger reads may wait.
Bulk byte caps bound input size, not transaction duration; account for supported
import sizes and database capacity when choosing limits. Neither the connection
inactivity timeout nor the connection-establishment cap is a transaction deadline.

## Kubernetes, Traefik and CNPG

The images can run in pods, but this repository ships no Kubernetes, Traefik or CNPG operators/manifests. Create separate backend/web Services targeting HTTP 8080. For one external HTTPS hostname, configure these ingress paths without stripping or rewriting:

| Path type | Paths | Service |
| --- | --- | --- |
| Prefix | `/auth`, `/users`, `/groups`, `/admin`, `/admin-ui`, `/currencies` | backend |
| Exact | `/swagger.json` | backend |
| Prefix | `/` | web |

More-specific API paths win. Preserve paths, queries, headers, same-origin admin cookies and XSRF. Unknown APIs must not fall back to a static index. Traefik owns TLS, body limits and ingress policy. Disable sensitive-router access logs or explicitly exclude query/auth header/body data; default logging is not necessarily secret-safe. Preserve backend authentication responses' no-store/no-referrer protections.

### Public ingress safeguards

Keep backend private to ingress with loopback bindings, a private service/firewall or network policy. Configuring `TRUSTED_PROXY_CIDRS` alone does not prevent direct access. Trust only actual ingress peers and known proxy-chain CIDRs, never arbitrary clients, all private networks or all Internet addresses. Direct launches need no proxy configuration.

At the public edge, remove untrusted incoming forwarding metadata or append the actually observed peer through an explicitly trusted chain. Backend uses the socket peer unless it is trusted; for a trusted peer it accepts at most one X-Forwarded-For field, 4096 bytes and 16 numeric IPs. It walks right-to-left only through trusted hops, ignoring Forwarded and X-Real-IP. IPv4-mapped IPv6 peers share IPv4 identity/range matching. Do not publish internal addresses or log forwarded chains, credentials, authentication queries or request bodies. Preserve paths, queries, admin cookies/XSRF and sensitive-response headers.

Before forwarding, enforce a starting general edge policy of 10 requests/second per source IP with burst 120, at most 32 concurrent API requests per source IP, maximum body 8 MiB and maximum total body-read duration 30 seconds. Also bound concurrent client connections and slow-client timeouts at the listener. Backend's finer auth/ordinary body classes and account rules remain authoritative. An operator may choose stricter ingress limits only if the lower upload capacity is acceptable. Valid CORS preflights may terminate before backend admission, so perimeter gates must cover them too.

For operator-owned Traefik, configure and attach these middlewares using the deployed version's supported configuration, with rate and in-flight gates before buffering:

| Middleware | Starting configuration |
| --- | --- |
| [RateLimit](https://doc.traefik.io/traefik/reference/routing-configuration/http/middlewares/ratelimit/) | `average=10`, `period=1s`, `burst=120` |
| [InFlightReq](https://doc.traefik.io/traefik/reference/routing-configuration/http/middlewares/inflightreq/) | `amount=32`; exhaustion returns 429, unlike backend concurrency 503 |
| [Buffering](https://doc.traefik.io/traefik/reference/routing-configuration/http/middlewares/buffering/) | `maxRequestBodyBytes=8388608`, `memRequestBodyBytes=1048576`; leave `retryExpression` unset |

For both limiters explicitly select `sourceCriterion.ipStrategy`, not request-host or arbitrary-header grouping. At the directly exposed edge, use `depth=0` to group by the observed remote IP. Behind another trusted load balancer, select only its verified forwarded chain using an explicitly configured depth or excludedIPs strategy; positive depth overrides excludedIPs. Never accept client-selected forwarding as the grouping key. Traefik appends the immediately preceding hop late in proxying, after middleware evaluation, so that hop is not yet available in the middleware's X-Forwarded-For chain.

Configure the edge's request-read deadline to 30 seconds and bound buffering memory and temporary disk at deployment level. Buffering is not a replacement for backend enforcement. Preserve proxy/backend failure statuses and any supplied Retry-After. Neither proxy nor clients may automatically replay mutations, including after network errors. This repository installs none of these ingress controls and supplies no version-specific CRDs.

For same-origin operation, set backend `PUBLIC_API_URL=https://ledger.example.test`, `FRONTEND_URL=https://ledger.example.test/`, empty `CLIENT_ORIGINS`, and explicitly empty web `API_BASE_URL`. Keep `ADMIN_COOKIE_SECURE=true` even when Traefik terminates TLS. Frontend configuration must never contain provider secrets or keys.

Connect to CNPG's read-write Service, such as `ledger-db-rw` or its namespace-qualified DNS name, never a pod IP or read-only Service. Use the application-owner Secret URI as `DATABASE_URL`, or PGHOST/PGPORT/PGDATABASE/PGUSER/PGPASSWORD with empty `DATABASE_URL`. Never use CNPG superuser credentials or silently rewrite its Secret URI. Set `PGSSLMODE=verify-full`, mount the CA read-only and point `PGSSLROOTCERT` to it. Explicit DSN fields override matching PG defaults.

Run backend as UID/GID 10001 with a read-only root filesystem, `allowPrivilegeEscalation=false`, all capabilities dropped, and `/tmp` emptyDir if needed. Web uses UID/GID/fsGroup 1000 with the same restrictions, read-only assets and only `/public/runtime` writable via emptyDir. Do not use outside-root symlinks, root startup or privileged init containers. No application PVC is needed; CNPG owns database persistence. Compose also drops all application capabilities, enables no-new-privileges and uses read-only roots with temporary mounts.

### Health probes and shutdown

Docker backend health uses bounded DB-backed `GET /auth/options`. Web health checks `/health`, `/` and the runtime file without contacting the API. Docker health is not a Kubernetes probe; database connection recovery belongs to the backend's connection manager.

Exact bodyless `GET /currencies` bypasses backend rate/admission gates and remains liveness-safe under overload. Other methods or bodies do not bypass them. DB-backed `GET /auth/options` is outside the strict authentication bucket but receives ordinary IP/global protection and can fail readiness under overload. Never exempt requests by User-Agent or a client-supplied health header. Existing image healthcheck and Kubernetes probe routes need no changes.

Configure Kubernetes probes explicitly; Docker HEALTHCHECK is not imported:

| Component | Startup / readiness | Liveness |
| --- | --- | --- |
| backend | `GET /auth/options` (DB-backed) | `GET /currencies` (in-memory, DB-independent) |
| web | `GET /runtime/api-base-url.txt` | `GET /health` |

Allow 180 seconds for startup. Readiness uses period 10s, timeout 5s and failure threshold 3. After startup, liveness uses period 30s, timeout 5s and failure threshold 3.

Both images exec the service as PID 1 and use SIGTERM. Backend handles SIGTERM/SIGINT with a 25-second graceful drain and closes its PostgreSQL connection on exit/failure. Compose uses `stop_grace_period: 30s`; pods need `terminationGracePeriodSeconds >= 30`. Interrupted mutations are not replayed.

### Operational limits

Backend retains at most one managed PostgreSQL connection. Connection failures
discard it and return a sanitized, noncached HTTP 503. A later database action,
including `/auth/options` readiness, makes one connection-establishment attempt
using the original DSN and current Service resolution. Establishment is capped at
five seconds, including at startup, even if libpq's configured timeout is longer.
Failed establishment leaves the slot available for another later request; there
is no background retry loop. Startup database failure exits for the supervisor
to restart. Schema initialization, authentication reconciliation and admin
bootstrap run only at process startup, never during reconnection.

Once PostgreSQL or the CNPG read-write Service is available again, later requests
can recover without restarting the backend. `/currencies` liveness stays
database-independent during the outage. This does not guarantee uninterrupted
requests or a deadline for detecting a broken in-flight network connection;
configure network and PostgreSQL timeouts for the deployment. Requests waiting
for the serialized slot can also wait behind another action or establishment
attempt.

The backend never automatically replays a failed action, transaction, commit or
rollback. A failed mutation response can have an unknown commit outcome: refresh
the ledger before deciding whether to submit again. Proxies and clients must
not automatically replay it. Exercise a CNPG switchover in staging and confirm
readiness recovery and a new ledger write with the same backend process. Retain
one backend replica initially; increasing database concurrency is a separate
change.

Initially use one backend replica and Recreate for authoritative authentication changes, not a mixed-configuration rolling rollout. Secret/ConfigMap environment changes require pod recreation. Follow [configuration ownership](authentication.md#configuration-ownership) for consistent configuration, key stability and ownership hand-back. Key rotation, database password rotation and data migrations are separate operator actions.
