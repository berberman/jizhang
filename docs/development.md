# Local development

Run commands from the repository root unless a section says otherwise. This guide
covers the backend and local testing of both the Flutter web client and the
same-origin admin console. Flutter-specific commands and configuration live in
the [client README](../client/README.md).

## Pinned environment

Install Nix with flake support. `flake.lock` pins the environment; the default
shell includes Flutter web, matching Dart, PostgreSQL, Cabal, and Haskell tools.
No manually installed Flutter SDK or native platform SDK is required for web
development. Enter the shell with:

```sh
nix develop --no-write-lock-file
```

Shell entry starts no services. `nix develop .#backend` omits Flutter and
PostgreSQL; use the default shell for the PostgreSQL commands below.

## Local launcher

With Nix installed, run:

```sh
./scripts/run-local.py
```

The launcher's Nix shebang enters the pinned shell. It builds the backend and
Flutter release, starts an isolated PostgreSQL instance, and waits for both HTTP
services to be ready:

- Flutter frontend: <http://127.0.0.1:3000>
- API: <http://127.0.0.1:8080>
- Admin console: <http://127.0.0.1:8080/admin-ui>

Register a normal user in the frontend. For optional admin access, supply both
`ADMIN_USERNAME` and `ADMIN_PASSWORD` in the environment before launching. There
is no default admin password; normal users and admins are separate account types.
See [authentication](authentication.md) for account and session behavior.

Both HTTP services bind to loopback by default. PostgreSQL remains loopback-only
on port 55432 with trust authentication. This is an HTTP testing setup, not a
production deployment: the launcher explicitly disables Secure admin cookies.

For another device on a trusted network, explicitly set
`JIZHANG_HOST=<operator-supplied-remote-host>` to a local interface address. Open
`http://<operator-supplied-remote-host>:3000`; API and admin use port 8080. Configure
the trusted network and firewall yourself; the launcher changes neither. Do not
expose this setup to an untrusted network. Production requires HTTPS and Secure
admin cookies.

`JIZHANG_PG_PORT` selects another isolated PostgreSQL port if 55432 is occupied;
5432 is rejected. Frontend/API ports remain 3000/8080. An unavailable configured
interface, occupied port, or concurrent launcher aborts startup without stopping
unrelated services. Ctrl-C stops only services started by the launcher.

### Persistent local data and schema limits

PostgreSQL data and logs persist in ignored `.local/jizhang/` under the repository
root, regardless of your working directory. Keep this directory across launches.
The launcher initializes the cluster and creates `jizhang_local` and `jizhang_test`
only when absent. Later runs retain accounts, groups, and ledger history.
Inherited `DATABASE_URL` and `CLIENT_ORIGINS` are ignored by the launcher.

The current schema requires an explicit `record_kind` with no default. Startup
adds the nullable record `note` column to existing current-schema databases
without changing rows or saved allocations; repeated startup is safe. This is
not a general migration mechanism for unrelated older schemas. Startup neither
infers legacy record kinds nor automatically resets a database. Never reset an
unrelated database or delete its PostgreSQL cluster. Recreate an older schema
only after confirming that it is disposable.

Keep private data, credentials, and development notes in ignored
`.local/private/`, not public source or examples.

## Manual backend setup

### Disposable PostgreSQL

Use the default pinned shell, a separate directory, and a free port. Do not run
these commands against an existing database. Stop any launcher using port 55432
before starting this instance. Keep the shell open so `PGROOT` remains available
for shutdown.

```sh
PGROOT=$(mktemp -d "${TMPDIR:-/tmp}/jizhang.XXXXXX")
initdb -D "$PGROOT/data" -U postgres -A trust --no-locale --encoding=UTF8
pg_ctl -D "$PGROOT/data" -l "$PGROOT/postgres.log" \
  -o "-h 127.0.0.1 -p 55432 -k $PGROOT" -w start
createdb -h 127.0.0.1 -p 55432 -U postgres jizhang_smoke
createdb -h 127.0.0.1 -p 55432 -U postgres jizhang_test
export DATABASE_URL='host=127.0.0.1 port=55432 user=postgres dbname=jizhang_smoke'
export TEST_DATABASE_URL='host=127.0.0.1 port=55432 user=postgres dbname=jizhang_test'
```

Trust authentication is only for this disposable, loopback-bound instance.
Preserve the directory until its data is no longer needed. Stop PostgreSQL
explicitly when finished:

```sh
pg_ctl -D "$PGROOT/data" -m fast -w stop
```

### Backend and admin console

In the configured Nix shell, with `DATABASE_URL` set as above:

```sh
export BIND_HOST=127.0.0.1
export ADMIN_USERNAME=local-admin
export ADMIN_PASSWORD=local-admin-password
export ADMIN_COOKIE_SECURE=false
export CLIENT_ORIGINS=http://127.0.0.1:3000
cabal run jizhang
```

These are synthetic credentials for a disposable local instance. Bootstrap
credentials create the configured admin only if absent; they do not reset an
existing password. Use a suitable password for any non-disposable deployment.

The API is at <http://127.0.0.1:8080>, the same-origin admin console at
<http://127.0.0.1:8080/admin-ui>, and Swagger at
<http://127.0.0.1:8080/swagger.json>. Launch with Cabal so packaged schema and UI
data paths are available. For manual backend launches, `BIND_HOST` controls the
listening address; leaving it unset retains Warp's all-interface default, unlike
the explicit loopback setting above.

`ADMIN_COOKIE_SECURE` defaults to `true`; only the explicit value `false` permits
local HTTP cookies. Other values fail startup. `CLIENT_ORIGINS` is a
comma-separated list of exact origins with whitespace trimmed; wildcards fail
startup. `localhost` and `127.0.0.1` are different origins. CORS applies only to
bearer user routes. Admin cookies and XSRF remain same-origin, so keep the admin
console with its API. See [authentication](authentication.md) for the remaining
backend authentication configuration.

The executable logs at Info. Ordinary HTTP requests produce one completion
message with method, path and status; routine reads and SQL require Trace.
Queries, headers and bodies are omitted from access logs. Authentication routes
log only completion status, and server failures use Attention.

## Verification

Backend integration tests start real servers and drop/create PostgreSQL tables.
Run them serially against a separate disposable database named `jizhang_test`
only, never the `jizhang_local` app database or unrelated data. Keep the app's
`DATABASE_URL` and the tests' `TEST_DATABASE_URL` separate.

After creating the test database, run in the pinned shell:

```sh
export TEST_DATABASE_URL='host=127.0.0.1 port=55432 user=postgres dbname=jizhang_test'
cabal build
cabal test jizhang-tests --test-options='--num-threads=1'
```

GitHub Actions runs the backend build and test suite and the Flutter tests and
web build on pushes and pull requests. The [workflow](../.github/workflows/ci.yml)
uses the pinned Nix environments and a disposable `jizhang_test` PostgreSQL
database. It does not deploy applications or require repository secrets.
Backend test output is shown only on failure; default test servers log at
Attention, while diagnostic-privacy tests explicitly exercise more verbose levels.

For Flutter doctor, analysis, tests, and release builds, use the
[client verification commands](../client/README.md#verification).

### Haskell API documentation

Module and declaration comments describe the backend contracts alongside the
implementation. Generate browsable library documentation with source links
from the pinned shell:

```sh
cabal haddock lib:jizhang --haddock-internal --haddock-hyperlink-source
```

Cabal prints the generated HTML location. The linked source includes comments
on validation, saved allocations, authentication and database transaction scope.

### Browser smoke scenario

Inspect the served release UI, keyboard access, and supported screen sizes before
deployment. Use only synthetic accounts and records:

1. In a fresh browser session, create Alice and Bob accounts and a shared group.
2. Create a 90-unit expense paid by Alice, with split weights Alice 1 / Bob 2.
   The balances should be +60 for Alice and −60 for Bob.
3. Reload a protected hash route. Sign-in must be required, then return to the
   intended route after authentication.
4. Cross-check the saved values in the admin console.

See [ledger behavior](ledger.md) for the financial, query, and editor contracts
shared by both frontends, and
[session and failure behavior](authentication.md#session-and-failure-behavior)
for authentication failures.
