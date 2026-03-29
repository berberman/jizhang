# AGENTS.md
Guidance for coding agents working in this repository.
This is based on the actual repo layout, Cabal config, source files, and tests.

## Repo Snapshot
- Language: Haskell2010 (`jizhang.cabal`)
- Build tool: Cabal
- Dev shell: Nix flake (`flake.nix`)
- App shape: Servant API + Beam/Postgres + JWT auth + Tasty tests
- Library code: `src/Jizhang/**`
- Executable entrypoint: `app/Main.hs`
- Tests: `test/*.hs`, runner in `test/Main.hs`

## Rule Files Present
- `AGENTS.md`: present
- `.cursorrules`: not present
- `.cursor/rules/`: not present
- `.github/copilot-instructions.md`: not present
Do not assume hidden editor-specific rules beyond this file.

## Setup Notes
Preferred environment: `nix develop`
Why: provides `cabal-install`, `haskell-language-server`, and `postgresql`.
Environment variables used by the app/tests:
- `DATABASE_URL` in `app/Main.hs`
- `TEST_DATABASE_URL` in `test/APITest.hs`
Defaults if unset:
- app DB: `host=localhost dbname=jizhang`
- test DB: `dbname=jizhang_test`
Tests expect PostgreSQL to be available.

## Build Commands
Primary build: `cabal build`
Useful targeted builds:
- `cabal build lib:jizhang`
- `cabal build exe:jizhang`
- `cabal build test:jizhang-tests`
- `nix build`
Run the server: `cabal run jizhang`
Notes:
- There is no Stack config; prefer Cabal.
- `jizhang.cabal` enables `-Wall` globally.
- Treat warnings as meaningful even though `-Werror` is not enabled.

## Test Commands
Run everything:
- `cabal test`
- `cabal test jizhang-tests`
Build tests without running:
- `cabal build test:jizhang-tests`
Run a single test or subset with Tasty patterns:
- `cabal test jizhang-tests --test-options="--pattern 'Report'"`
- `cabal test jizhang-tests --test-options="--pattern 'calculateBalance'"`
- `cabal test jizhang-tests --test-options="--pattern 'Auth / register and login'"`
Pattern source of truth:
- top-level groups in `test/Main.hs`
- API integration groups/cases in `test/APITest.hs`
- pure report tests in `test/ReportTest.hs`
Important test behavior:
- `test/APITest.hs` boots a real Warp app
- integration tests create/drop Postgres tables
- report tests are pure and cheaper than API tests

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
- Tables are created with raw SQL in `app/Main.hs` and test setup, not migrations.
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
- Reuse helpers such as `withTestApp`, `runClient`, `runClientExpectStatus`, and `assertApproxEqual`.
- If behavior changes at the API layer, update the relevant tests in `test/APITest.hs` or `test/ReportTest.hs`.

## Change Strategy
- Make minimal, local changes.
- Preserve module boundaries.
- Extend existing helpers before copying logic.
- Match the touched file's pragma/import/layout style.
- Avoid broad cleanups unless explicitly requested.

## Files and Artifacts to Avoid Committing
From `.gitignore`:
- `dist-newstyle`
- `cabal.project.local`
- `result`
- `result-bin`
- `links`
- `jizhang.db`

## Verification Order
Prefer this sequence before finishing:
1. `cabal build`
2. the narrowest relevant `cabal test ... --test-options="--pattern '...'"`
3. `cabal test` if shared/auth/DB behavior changed
4. Always run tests after making changes. If the relevant tests cannot pass because of pre-existing failures or environment issues, still run the narrowest relevant test command and report the blocker explicitly.
