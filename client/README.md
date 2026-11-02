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

Web builds ship `runtime/api-base-url.txt` containing `same-origin`, which sends
API requests to the browser origin. Preserve this file when copying built assets.
To use a separate API host or a path prefix, replace the token with an absolute
HTTP(S) URL such as `https://example.org/api`, without credentials, query or
fragment. Surrounding file whitespace is allowed; empty content is invalid.

Web startup fetches the file relative to the browser URL, including a subdirectory
deployment. The request uses a fresh `_runtime` cache key, no-cache, no redirects
and a five-second timeout. The file must be strict UTF-8 plain text. Failed
fetches, non-200 responses, invalid UTF-8 and invalid values stop startup before
ordinary API requests, without falling back to another API address.

For development or advanced static builds, a nonempty compile-time `API_BASE_URL`
overrides the runtime file. It must be an absolute HTTP(S) URL with the same URL
restrictions; a path prefix is retained. Rebuild to change a compiled value.
Non-web launches do not fetch the runtime file and retain their existing
compile-time configuration behavior.

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

## Receipt images

New receipts support photo upload and browser camera capture. Upload accepts
JPEG, PNG, WebP, and HEIC/HEIF, then shows the normalized JPEG that will be sent
and retained. HEIC/HEIF decoding runs locally in a dedicated worker using the
packaged libheif-js decoder; native browser HEIF support is not required. No photo
is uploaded on selection. Camera access starts only after **Take photo** and
requires HTTPS or localhost plus browser permission. Unsupported or denied camera
access retains the upload and manual-entry alternatives.

Original files are limited to 20 MiB. HEIF preparation allows at most 16,384 pixels
per edge and 64 million original pixels, with a 15-second deadline. Prepared
images use a white background and JPEG quality 0.92, with a 2,560-pixel long edge
and the server's configured byte cap. Invalid, oversized, unavailable-decoder, and
timeout errors retain the previous photo and editable draft. Leaving the form
terminates an active decoder.

Deploy the complete web output, including `receipt-heif.js`, its worker, and
`vendor/libheif/`. Decoder notices and rebuild instructions appear in
**Open-source licenses**; the adjacent corresponding-source archive is included
with the web distribution. See [distribution requirements](../docs/licenses.md).

Selecting a photo does not call the provider. **Read receipt** is the explicit
billable action; review and correction are local and do not call it again.
Review starts with item expenses. Choose a tip option and the eligible items for
each added or subtracted charge, then choose **Continue** to accept the displayed
values. **Receipt details** offers one-expense recording, including deliberate
bill-only recording when the actual tip is unknown. Warnings require one explicit
acknowledgment when present. Confirm the displayed payers and splits before saving.

Saving commits the source and expenses together. Failed extraction/saves retain
the draft; leaving an in-flight extraction may still incur provider charges.
Saved sources open only on demand and their import review is historical, not a
rule for recalculating saved amounts or exchange rates. No source image or
evidence draft is persisted in browser storage. See [receipt contracts](../docs/ledger.md#receipts-and-csv-imports)
and [server configuration](../docs/configuration.md#receipt-image-extraction).


## Native targets

Only Flutter web packaging is provided. Android, iOS, macOS, Windows and Linux
native packaging, platform SDK setup, native lifecycle/back integration and
secure native session restoration are not provided.
