# Jizhang

Jizhang is a self-hosted shared expense, income and payment ledger with a Flutter
web client, a same-origin admin console and a Haskell Servant API backed by PostgreSQL.

- Track expenses, shared income and payments using equal, weighted, percentage
  or exact splits.
- Keep money exact with integer minor units, currency-aware precision and saved
  exchange-rate allocations.
- Review member balances, suggested settlements, searchable records and dated
  statistics. Receipts bundle expenses; CSV imports are expense-only.
- Sign in with a local account or configured OIDC provider. Operator accounts
  are separate from ordinary users.

## Run locally

Install [Nix](https://nixos.org/download/) with flake support, then run from the
repository root:

```sh
./scripts/run-local.py
```

The launcher builds the applications, starts local PostgreSQL and serves:

- Web client: <http://127.0.0.1:3000>
- API: <http://127.0.0.1:8080>
- Admin console: <http://127.0.0.1:8080/admin-ui>

Register a normal user in the web client. Optional admin access requires both
`ADMIN_USERNAME` and `ADMIN_PASSWORD`; no default admin account is created.

This launcher is for local HTTP development, not production. It preserves data
between runs. See [local development](docs/development.md) for configuration,
manual setup and safe testing.

## Documentation

- [Local development and verification](docs/development.md)
- [Flutter client development](client/README.md)
- [Deployment](docs/deployment.md): container images, Compose, static hosting
  and Kubernetes integration
- [Advanced configuration](docs/configuration.md): optional environment overrides
- [Authentication](docs/authentication.md): accounts, OIDC, sessions and
  operator configuration
- [Ledger behavior](docs/ledger.md): currencies, records, queries, balances
  and statistics
- [Haskell architecture](docs/architecture.md): module ownership and
  dependency rules
- [Changelog](CHANGELOG.md)
- [Self-hosted data handling](docs/privacy.md): stored data, access, and external services
- [Open-source notices](docs/licenses.md): client notices and distribution requirements

## License

[MIT](LICENSE).
