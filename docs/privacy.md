# Self-hosted data handling

Jizhang is self-hosted software. Each instance's operator controls its server,
accounts, access, backups, and retention. This guide describes application
behavior, not a privacy policy or a promise on behalf of every installation.

## Stored information and access

PostgreSQL stores account profiles, password hashes, authentication/session state,
groups, memberships, and shared financial records. Records include titles, notes,
dates, amounts, currencies, splits, and saved exchange-rate provenance. Receipt
bundles contain expenses, not uploaded images. Submitted CSV text reaches the
backend for import.

Group members can see their authorized shared ledger, including historical
participant references. Ordinary shared identities do not expose account email or
verification status. Group owners can discover public user identities to add
members. Administrators and anyone with database or backup access can access
stored information. Jizhang does not provide end-to-end encryption.

The Flutter client keeps bearer credentials and working drafts in memory. A
reload requires sign-in. External sign-in temporarily stores redirect-verification
state in the originating tab's `sessionStorage`. The admin console uses
same-origin authentication and anti-forgery cookies. See
[Authentication](authentication.md) for session and sign-out behavior.

## External requests

- Configured OIDC sign-in sends authentication traffic to the selected provider.
  The provider supplies identity claims; ledger contents are not part of that
  authentication exchange.
- Reference-rate lookup sends a currency pair from the backend to Frankfurter at
  `api.frankfurter.dev` when a provider lookup is needed. It does not send account
  identities, amounts, group details, or record notes.
- Browser asset delivery depends on the web build and deployment. Review its
  network traffic, including Flutter engine/font resources, and any hosting,
  reverse-proxy, analytics, or monitoring services you add. Self-hosting the API
  alone does not establish that every browser request stays on your infrastructure.

## Operator responsibilities

Choose and secure the infrastructure, restrict administrator access, configure
HTTPS and authentication, and protect secrets and backups. Review provider,
network, and diagnostic logging. Routine access logs contain completion status
and ordinary request method/path; diagnostic verbosity can expose additional
identifiers and ledger data. Set retention and backup-expiry rules appropriate to
your instance rather than assuming the application periodically erases records.

Signing out, disabling an account, and removing a member do not delete financial
history. Account deletion can conflict with retained group or financial references;
there is no automatic anonymization or recall of copies kept by other members.
Decide how your instance handles correction, deletion, and shared-history requests.

If you give other people access, explain your actual data handling and meet any
applicable legal duties. The repository supplies no universal hosted-service Terms
of Use, consent gate, or operator-specific Privacy Notice. See
[Deployment](deployment.md) for configuration and operational guidance.
