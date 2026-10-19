# Haskell architecture

The backend separates HTTP orchestration, feature policy, pure calculations and
persistence. `Jizhang.API.Types.*` contains the shared request/response types;
`API.Types` is their DTO-only facade.

## Module ownership

- `API.<Feature>` defines routes and handlers. Handlers authorize, load data,
  resolve requests, call persistence and project responses.
- `API.User.Account` owns account normalization, password validation and
  provisioning. `API.Record.Input` owns financial request resolution, allocations,
  currency policy and exchange-rate provenance.
- `API.User.Projection`, `API.Group.Projection` and `API.Record.Projection`
  convert stored values to responses and validate stored invariants without IO.
- `API.Record.Service` provides shared ledger loading, whole-ledger queries and
  atomic imports for authorized callers. Other handlers use this service rather
  than importing record route implementations.
- `Report.Balance` and `Report.Statistics` contain pure ledger folds;
  `Report.Markdown` renders reports. They do not load records or authorize users.
- `Database.*` owns Beam queries and writes; `Database.Schema` owns table mappings.
  Persistence consumes resolved financial values rather than recalculating money.
- `API.App` owns the handler environment, connection serialization, transaction
  policies and SQL error mapping. `API.Internal.Resource` centralizes resource
  lookup/authorization; `API.Internal.Error` converts failures to HTTP responses.

## Dependency rules

Dependencies point from handlers to feature policy and persistence, not back into
route modules. Projections and reporting folds must not depend on handlers,
runtime or database execution. Financial arithmetic remains in `Jizhang.Money`;
record query parsing/evaluation remains in `Jizhang.RecordQuery`.

Cabal keeps account, input, projection and internal helper modules private using
`other-modules`; they are not library extension points. Each declares an explicit
export list. Import the runtime from `API.App`, not the DTO facade. Add new policy
under its feature namespace rather than recreating a general utility module.

See [local development](development.md) for backend setup and verification, and
[ledger behavior](ledger.md) for the financial and query contracts.
