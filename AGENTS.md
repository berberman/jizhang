# AGENTS.md

Guidance for coding agents working in this repository. Keep public examples
synthetic (for example Alice/Bob); never publish real member mappings, source
worksheets, private network addresses, credentials, or local dataset totals.
Author/license attribution is not a group-membership claim and must be retained.
Keep public docs free of execution diaries, machine-specific endpoints,
credentials, test-count receipts, and temporary troubleshooting narratives.
All `.local/` content is ignored: runtime state belongs in `.local/jizhang/`,
private material and agent execution notes in `.local/private/`.

## Repository and environment

- Haskell2010 / Cabal: `src/Jizhang/**`, executable `app/Main.hs`, tests `test/*.hs`.
- Flutter client: `client/lib`, behavior/model tests `client/test`.
- Same-origin admin console: `ui` (plain JavaScript/CSS, served by the API).
- Schema source: `Jizhang.Database.Init` loads packaged `sql/schema.sql`; Beam
  mappings live in `Jizhang.Database.Schema`. Packaged UI/SQL paths require the
  normal Cabal launch environment.
- Local full-stack entry: `scripts/run-local.py`; public overview: `README.md`.
  Manual setup and verification: `docs/development.md`; deployment:
  `docs/deployment.md`; Flutter-specific development: `client/README.md`.
- `flake.lock` pins the default shell: `nix develop --no-write-lock-file` includes
  Flutter, matching Dart, PostgreSQL, Cabal, and Haskell tooling. The explicit
  `nix develop .#backend` shell omits Flutter. Neither starts services.
- The launcher defaults HTTP to `127.0.0.1` and preserves `.local/jizhang/`;
  explicit `JIZHANG_HOST` overrides are for operator-configured trusted testing.
  Direct backend launches use `BIND_HOST` (unset retains Warp's default).

## Shared financial and frontend contracts

Both Flutter and the admin console consume the same API. Keep their models,
editors, validation, error behavior, and fixtures consistent when contracts change.
Render user text literally; do not interpret it as HTML or Markdown. Preserve
UUID-backed selections, historical identities, pending guards, dirty drafts, and
failed-save input. Normal bearer accounts and cookie-authenticated admin accounts
are separate; public identities must not expose private email.

Money uses Haskell integers and Dart/JavaScript `BigInt`, with decimal-string API
minor amounts and catalog-defined currency precision. Never route money through
floating point or JavaScript `Number`. Preserve exact original/settlement amounts,
persisted split allocations, and saved exchange-rate provenance. Never recompute
saved allocations from current rates. Keep currencies separate in reports and
suggested payments. Receipts and five-column CSV imports remain expense-only;
income and transfers have distinct financial semantics.

### Record notes and queries

Record notes are nullable plain text for every kind, independent of receipt notes.
Requests may omit `note`; responses require the field as text or null. Full PUT
omission/null/blank clears it; preserve exact nonblank Unicode and newlines,
reject NUL before SQL, and retain notes on unrelated edits. Startup adds the
nullable column to the current schema without resetting rows or allocations.
Five-column CSV remains unchanged and supplies null record notes.

Both record endpoints use `Jizhang.RecordQuery` over the whole authorized ledger.
Admin records are server-paginated; scoped detail GET drives report/Stats actions.

- Record queries must be evaluated once on the server over the complete authorized
  group before pagination, consistently for both frontends. Terms combine with
  AND; quoting/escapes and qualifiers must reject malformed input, not broaden it.
  Never search private email. Identity selections serialize immutable UUIDs.
- Query qualifiers cover title, note, type, payer, payee, member, transaction date,
  UTC created day, original currency, and exact original amount. Amount comparisons
  require one unambiguous currency and catalog precision; do not convert currencies.
- Record sorting defaults to descending transaction date, uses UUID-ascending
  ties independent of direction, and groups amount sorting by currency. Do not
  silently reuse generic list sorting or filter only a loaded page.
- Preserve raw draft query separately from applied results; inline errors retain
  editable input and clearly label previous results. Scope caches to group/session.

### Balance reports

Extend the existing all-time report fold, never reconstruct role totals from
paginated records or dated statistics. `Balance` requires six nonnegative exact
role totals; net reconciles as paid − share − income received + entitlement +
sent − received. Breakdown merges impacts per record without doubling saved
gross amount, retains zero impacts, and sorts date descending / UUID ascending.
Flutter report decoding is strict; update all report fixtures on contract changes.
Member sorting is exact and local, with UUID-ascending ties in either direction.
Retain currency, sort and expanded currency+UUID state; suggested payments stay
server-ordered and review-only. No database changes are required for role totals.

### Statistics presentation

Keep the backend's top-five allocated-expense order and exact Other remainder;
do not invent categories or recompute saved allocations. Percentages truncate
integer basis points; only bounded display ratios convert to floating point.
Other and total never have record actions. Zero share has no dummy expense bar;
group income/net stay visible and payments remain excluded.
Flutter uses `_SpendingDataTable`; admin uses semantic scoped column headers.
All time buckets and members belong in tables, independent of chart truncation.
Keep table-local horizontal scrolling and separate disclosure/scroll/text
PageStorage keys. Scoped expense actions must remain keyboard reachable.


## Build and verification

From the default pinned shell:

```sh
cabal build
# Point this at a separately created disposable database named jizhang_test.
export TEST_DATABASE_URL='host=127.0.0.1 port=55432 user=postgres dbname=jizhang_test'
cabal test jizhang-tests --test-options='--num-threads=1'
cd client
flutter pub get
flutter test
flutter build web --dart-define=API_BASE_URL=http://127.0.0.1:8080
```

API integration tests start real Warp servers and drop/create PostgreSQL tables.
Run them serially (`--num-threads=1`) on **`jizhang_test` only**, never the launcher
app database or unrelated user data. The app's `DATABASE_URL` and tests'
`TEST_DATABASE_URL` are separate; do not rely on defaults for destructive runs.
Use the disposable-cluster instructions in `docs/development.md`. Tasty group/case
names in `test/Main.hs` and the relevant test file are the source of truth for
narrow `--pattern` selections; do not copy obsolete patterns.

After contract integration, run the affected commands, then exercise the real
application. Web changes require actual-browser inspection of both Flutter and
admin surfaces, including mobile widths, keyboard interaction, and enlarged
text. A unit test or successful build is not visual proof. Report only observed
verification and blockers; do not claim native-platform coverage from a web run.
Do not reset persistent local data to make verification pass.

## Lint / Formatting
No dedicated formatter or linter config is checked in.
Not found: `.hlint.yaml`, `fourmolu.yaml`, `ormolu.yaml`, `.stylish-haskell.yaml`, `.editorconfig`.
Practical guidance:
- use `cabal build` as the baseline verification step
- preserve existing manual formatting
- avoid large repo-wide formatting churn

## Architecture
Keep the existing module split:
- `Jizhang.API.*`: Servant route types and handlers
- `Jizhang.API.Types.*`: API request/response/domain types
- `Jizhang.Database.*`: Beam query helpers
- `Jizhang.Database.Schema`: Beam tables and DB naming mappings
Do not collapse these layers unless explicitly asked.
Two type worlds matter here:
- schema types live in `Jizhang.Database.Schema` and use Beam-parameterized `*T f` records with `_`-prefixed fields
- API types live in `Jizhang.API.Types.*` and are plain records/newtypes for JSON, Swagger, and Servant boundaries

## Code Style
### Formatting
- Use 2-space indentation.
- Put language pragmas at the top, one per line.
- Add LANGUAGE pragmas per file, not project-wide.
- Preserve the touched file's existing spacing and wrapping.
- Long Servant type aliases and record literals are often split across lines.
- Keep export lists vertically aligned when a file already uses them.

### Imports
- Standard/third-party imports come before local `Jizhang.*` imports.
- Qualified imports are common when they improve clarity, e.g. `Data.Text as T`, `Data.Map.Strict as M`, `Jizhang.Database.Schema as S`.
- Database helpers are commonly imported as `import qualified Jizhang.Database as D` and schema as `import qualified Jizhang.Database.Schema as S`.
- Match the touched file's import style instead of reordering unrelated imports.
- Use selective imports when the file already prefers them.

### Naming
- Modules use hierarchical PascalCase: `Jizhang.API.Record`.
- Values/functions use camelCase: `runDB`, `getGroupUserMap`, `validateGroupName`.
- Data types and constructors use PascalCase.
- Prefer descriptive newtypes for IDs/wrappers: `UserId`, `GroupId`, `Username`, `ReceiptId`.
- DB schema fields use leading underscores: `_userId`, `_groupOwner`, `_createdAt`.
- API-facing record fields do not use underscores: `userId`, `username`, `authUserId`.

### Types and Data Modeling
- Reuse existing `Jizhang.API.Types.*` types instead of introducing raw `UUID`/`Text` at API boundaries.
- Prefer newtypes for identifiers and captures.
- Prefer `coerce` when converting between aligned wrappers/schema IDs instead of manual wrapping/unwrapping.
- Keep strict fields (`!`) where surrounding code already uses them.
- Match existing deriving style: `deriving stock` for structural instances, `deriving newtype` for wrappers, `deriving anyclass` for JSON/Swagger/JWT classes when already used.
- Keep Beam schema definitions in `Jizhang.Database.Schema`.

### Servant / Handler Conventions
- Define routes as `type ...API = ...` aliases.
- Keep server assembly separate from endpoint implementations.
- Authenticated handlers typically take `AuthUser` first.
- Use `MyHandler` for handlers and `runDB` for DB work.
- Reuse helpers from `Jizhang.API.Utils` like `ensureGroupExists`, `ensureGroupMember`, `fetchOrFail`, and validation helpers.
- Prefer small shared helpers over repeating validation logic inline.

### Error Handling
- Use `throwError` with explicit Servant errors such as `err400`, `err401`, `err403`, `err404`, `err409`, `err500`.
- Include a specific `errBody`.
- Validate inputs early with `when` / `unless`.
- Convert missing rows/lookups into HTTP errors near the boundary.
- Do not introduce partial functions unless the invariant is already established nearby.

### Logging and Database
- Use handler logging for meaningful actions (`logInfo_`).
- The standard execution model is `AppEnv` + `MyHandler` + `LogT`.
- `runDB` already logs SQL through `runBeamPostgresDebug`; do not add duplicate DB tracing casually.
- Keep table naming compatible with the mappings in `jizhangDb`.
- `Jizhang.Database.Init.createTables` executes packaged `sql/schema.sql` at startup and in test setup; update that SQL and Beam mappings together. Do not assume a general migration system.
- SQL names are snake_case, with some FK columns like `owner__id` and `uploaded_by__id`.
- Haskell Beam fields remain underscored camelCase.
- Keep Beam queries inside `Jizhang.Database.*` when possible rather than pushing them into handlers.

### Swagger
- Swagger is exposed at `/swagger.json`.
- API-facing types should keep `ToSchema` instances when appropriate.

### Tests
- Tests use `tasty` and `tasty-hunit`.
- Use `testGroup` with readable names.
- Prefer descriptive behavior-style test names.
- Reuse helpers such as `withTestAppServer`, `setupUser`, `fixtureUserId`, `runClient`, `runClientExpectStatus`, and `identityRequest`.
- If behavior changes at the API layer, update the relevant tests in `test/APITest.hs` or `test/ReportTest.hs`.

## Change Strategy
- Make minimal, local changes.
- Preserve module boundaries.
- Extend existing helpers before copying logic.
- Match the touched file's pragma/import/layout style.
- Avoid broad cleanups unless explicitly requested.
- README files are user/operator setup guides; CHANGELOG records externally
  observable unreleased/released behavior. Per-run evidence and plans stay local.

## Private and generated files

Root ignore rules cover all `.local/`, `.env`/`.env.*` (except a public
`.env.example`), `.DS_Store`, Python caches, Cabal outputs, and local build
configuration. Runtime state belongs in `.local/jizhang/`; private material and
agent execution notes belong in `.local/private/`. Keep private material
owner-only (directory 0700, files 0600); do not print its values in logs, tests,
or review output. Keep the persistent launcher workspace and unrelated user
changes intact. Keep this public `AGENTS.md` tracked; never add case-specific
local paths, member data, or session artifacts to it.

Retain the client ignore rules for Flutter/Dart/build/cache/IDE artifacts. Keep
intentional `client/pubspec.lock`, Flutter `.metadata`, and generic
`.vscode/settings.json`; do not delete them as a cleanup shortcut. Never stage
all files blindly. Scan publishable tracked and untracked text when cleaning
privacy-sensitive content; ignored local material is not publication input.
