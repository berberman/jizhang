# Jizhang Flutter client

Flutter web client for the Jizhang API. The same-origin admin console is a
separate interface served by the backend.

For the complete local stack, use the [project quick start](../README.md#run-locally).
Backend/database setup and destructive-test safeguards belong in the
[development guide](../docs/development.md).

## Development

From the repository root, enter the pinned shell and start the web development
server against a running local API:

```sh
nix develop --no-write-lock-file
cd client
flutter pub get
flutter run -d web-server --web-hostname=127.0.0.1 --web-port=3000 \
  --dart-define=API_BASE_URL=http://127.0.0.1:8080
```

Open <http://127.0.0.1:3000>. The pinned shell supplies matching Flutter and Dart
versions; a separate SDK installation is not required.

## API configuration

A nonempty compile-time `API_BASE_URL` takes precedence over runtime
configuration. It must be an absolute HTTP(S) URL without credentials, query or
fragment. A path prefix such as `https://example.org/api` is retained. Rebuild to
change a compiled value.

Without a compiled override, web startup fetches `runtime/api-base-url.txt`
relative to the browser URL, including a subdirectory deployment. The request
uses a fresh `_runtime` cache key, no-cache, no redirects and a five-second
timeout. The file is strict UTF-8 plain text; trimmed empty content explicitly
selects the browser origin.

Failed fetches, non-200 responses, invalid UTF-8 and invalid URLs stop startup
before ordinary API requests. Non-web launches do not fetch this file. Normal
non-container web builds ship an empty file for same-origin use; preserve it
when copying built assets.

For container runtime configuration, CORS, TLS and proxy routing, see the
[deployment guide](../docs/deployment.md).

## Verification

Run from `client/` inside the pinned shell:

```sh
flutter pub get
flutter doctor -v
flutter analyze
flutter test
flutter build web --dart-define=API_BASE_URL=http://127.0.0.1:8080
```

Builds and tests do not replace inspection of the served release UI. Follow the
[full-stack verification guide](../docs/development.md#verification) for browser
checks, keyboard interaction, screen sizes and admin cross-checks.

See [static web deployment](../docs/deployment.md#static-web-deployment) for
release hosting and a local release smoke server. Nix-built container images use
a separate, runtime-configured build workflow documented in the same guide.

## Behavior references

- [Ledger contracts](../docs/ledger.md): money, notes, record queries, CSV imports,
  balances and statistics shared by both interfaces
- [Authentication and sessions](../docs/authentication.md): sign-in, invitations,
  credential changes, in-memory credentials and reload behavior

Record details retain their originating Records, Statistics, Balances or receipt
view within the same signed-in session. Back restores its filters, drafts and
position; successful record changes refresh server data when that view returns.
Failed refreshes label previously loaded data and offer Retry. Direct detail
links without a retained predecessor return to Records. Pushed detail/editor
URLs appear in browser history; dismissing them consumes the pushed entry, and
Forward does not restore a saved editor.

Account keeps profile fields visible and groups less-used sign-in, identity and
deletion controls in disclosures. Profile drafts and leave protection remain
active while these sections are opened or closed. Credential changes still
require the existing proofs and retire application sessions.

## Native targets

Only Flutter web packaging is provided. Android, iOS, macOS, Windows and Linux
native packaging, platform SDK setup, native lifecycle/back integration and
secure native session restoration are not provided.
