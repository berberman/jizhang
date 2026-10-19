CREATE TABLE IF NOT EXISTS users (
  id UUID PRIMARY KEY,
  username TEXT NOT NULL UNIQUE CHECK (username ~ '^[a-z0-9_][a-z0-9_.-]{0,49}$'),
  display_name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE CHECK (email <> '' AND email = lower(email) AND email = btrim(email)),
  email_verified BOOLEAN NOT NULL DEFAULT FALSE,
  password_hash TEXT NULL,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  auth_invalidated_at TIMESTAMPTZ NULL
);

CREATE TABLE IF NOT EXISTS external_identities (
  issuer TEXT NOT NULL CHECK (issuer <> ''),
  subject TEXT NOT NULL CHECK (subject <> ''),
  user__id UUID NOT NULL UNIQUE REFERENCES users (id) ON DELETE CASCADE,
  linked_at TIMESTAMPTZ NOT NULL,
  last_login_at TIMESTAMPTZ NULL,
  claimed_email TEXT NULL,
  claimed_email_verified BOOLEAN NOT NULL DEFAULT FALSE,
  PRIMARY KEY (issuer, subject)
);

CREATE TABLE IF NOT EXISTS admins (
  id UUID PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS auth_settings (
  id SMALLINT PRIMARY KEY CHECK (id = 1),
  revision BIGINT NOT NULL DEFAULT 0 CHECK (revision >= 0),
  local_registration_enabled BOOLEAN NOT NULL DEFAULT TRUE,
  sso_registration_enabled BOOLEAN NOT NULL DEFAULT TRUE,
  oidc_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  provider_name TEXT NOT NULL DEFAULT 'Authentik' CHECK (length(provider_name) BETWEEN 1 AND 100 AND provider_name !~ '[[:cntrl:]]'),
  issuer TEXT NULL CHECK (issuer <> ''),
  client_id TEXT NULL CHECK (client_id <> ''),
  client_secret_encrypted TEXT NULL CHECK (client_secret_encrypted <> ''),
  CHECK (NOT oidc_enabled OR (issuer IS NOT NULL AND client_id IS NOT NULL AND client_secret_encrypted IS NOT NULL))
);
INSERT INTO auth_settings (id) VALUES (1) ON CONFLICT (id) DO NOTHING;

CREATE TABLE IF NOT EXISTS user_sessions (
  id UUID PRIMARY KEY,
  user__id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  method TEXT NOT NULL CHECK (method IN ('password', 'oidc')),
  created_at TIMESTAMPTZ NOT NULL,
  authenticated_at TIMESTAMPTZ NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL,
  oidc_reauthenticated_at TIMESTAMPTZ NULL,
  revoked_at TIMESTAMPTZ NULL,
  oidc_issuer TEXT NULL CHECK (oidc_issuer <> ''),
  oidc_subject TEXT NULL CHECK (oidc_subject <> ''),
  oidc_sid TEXT NULL CHECK (oidc_sid <> ''),
  id_token_encrypted TEXT NULL,
  CHECK (expires_at > created_at),
  CHECK ((method = 'oidc' AND oidc_issuer IS NOT NULL AND oidc_subject IS NOT NULL)
      OR (method = 'password' AND oidc_issuer IS NULL AND oidc_subject IS NULL AND oidc_sid IS NULL AND id_token_encrypted IS NULL AND oidc_reauthenticated_at IS NULL))
);
CREATE INDEX IF NOT EXISTS user_sessions_user_idx ON user_sessions (user__id);
CREATE INDEX IF NOT EXISTS user_sessions_sid_idx ON user_sessions (oidc_issuer, oidc_sid);
CREATE INDEX IF NOT EXISTS user_sessions_subject_idx ON user_sessions (oidc_issuer, oidc_subject);
CREATE INDEX IF NOT EXISTS user_sessions_expires_idx ON user_sessions (expires_at);

CREATE TABLE IF NOT EXISTS account_invitations (
  id UUID PRIMARY KEY,
  token_hash BYTEA NOT NULL UNIQUE CHECK (octet_length(token_hash) = 32),
  purpose TEXT NOT NULL CHECK (purpose IN ('password_reset', 'oidc_link')),
  user__id UUID NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  admin__id UUID NOT NULL REFERENCES admins (id),
  provider_revision BIGINT NULL CHECK (provider_revision >= 0),
  created_at TIMESTAMPTZ NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL,
  consumed_at TIMESTAMPTZ NULL,
  revoked_at TIMESTAMPTZ NULL,
  CHECK (expires_at > created_at AND expires_at <= created_at + INTERVAL '30 minutes'),
  CHECK ((purpose = 'password_reset' AND provider_revision IS NULL) OR (purpose = 'oidc_link' AND provider_revision IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS account_invitations_expires_idx ON account_invitations (expires_at);
CREATE INDEX IF NOT EXISTS account_invitations_user_idx ON account_invitations (user__id);

CREATE TABLE IF NOT EXISTS oidc_transactions (
  id UUID PRIMARY KEY,
  state_hash BYTEA NOT NULL UNIQUE CHECK (octet_length(state_hash) = 32),
  provider_revision BIGINT NOT NULL CHECK (provider_revision >= 0),
  purpose TEXT NOT NULL CHECK (purpose IN ('login', 'link', 'reauth', 'invitation')),
  user__id UUID NULL REFERENCES users (id) ON DELETE CASCADE,
  session__id UUID NULL REFERENCES user_sessions (id) ON DELETE CASCADE,
  invitation__id UUID NULL REFERENCES account_invitations (id) ON DELETE CASCADE,
  browser_challenge BYTEA NOT NULL CHECK (octet_length(browser_challenge) = 32),
  nonce_hash BYTEA NOT NULL CHECK (octet_length(nonce_hash) = 32),
  pkce_verifier_encrypted TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL,
  phase TEXT NOT NULL CHECK (phase IN ('waiting', 'exchanging', 'resolved', 'continued', 'consumed', 'failed')),
  handoff_hash BYTEA NULL UNIQUE CHECK (octet_length(handoff_hash) = 32),
  continuation_hash BYTEA NULL UNIQUE CHECK (octet_length(continuation_hash) = 32),
  result_payload_encrypted TEXT NULL,
  password_attempts INT NOT NULL DEFAULT 0 CHECK (password_attempts BETWEEN 0 AND 5),
  CHECK (expires_at > created_at AND expires_at <= created_at + INTERVAL '21 minutes'),
  CHECK ((purpose = 'login' AND user__id IS NULL AND session__id IS NULL AND invitation__id IS NULL)
      OR (purpose IN ('link', 'reauth') AND user__id IS NOT NULL AND session__id IS NOT NULL AND invitation__id IS NULL)
      OR (purpose = 'invitation' AND user__id IS NOT NULL AND session__id IS NULL AND invitation__id IS NOT NULL)),
  CHECK ((phase IN ('waiting', 'exchanging') AND result_payload_encrypted IS NULL AND handoff_hash IS NULL AND continuation_hash IS NULL)
      OR (phase = 'resolved' AND result_payload_encrypted IS NOT NULL AND handoff_hash IS NOT NULL AND continuation_hash IS NULL)
      OR (phase = 'continued' AND result_payload_encrypted IS NOT NULL AND handoff_hash IS NOT NULL AND continuation_hash IS NOT NULL)
      OR phase IN ('consumed', 'failed'))
);
CREATE INDEX IF NOT EXISTS oidc_transactions_expires_idx ON oidc_transactions (expires_at);
CREATE INDEX IF NOT EXISTS oidc_transactions_user_idx ON oidc_transactions (user__id);
CREATE INDEX IF NOT EXISTS oidc_transactions_session_idx ON oidc_transactions (session__id);
CREATE INDEX IF NOT EXISTS oidc_transactions_invitation_idx ON oidc_transactions (invitation__id);

CREATE TABLE IF NOT EXISTS oidc_logout_events (
  issuer TEXT NOT NULL CHECK (issuer <> ''),
  jti TEXT NOT NULL CHECK (jti <> ''),
  sid TEXT NULL CHECK (sid <> ''),
  sub TEXT NULL CHECK (sub <> ''),
  received_at TIMESTAMPTZ NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (issuer, jti),
  CHECK (sid IS NOT NULL OR sub IS NOT NULL),
  CHECK (expires_at >= received_at + INTERVAL '30 minutes')
);
CREATE INDEX IF NOT EXISTS oidc_logout_events_expires_idx ON oidc_logout_events (expires_at);
CREATE INDEX IF NOT EXISTS oidc_logout_events_sid_idx ON oidc_logout_events (issuer, sid);
CREATE INDEX IF NOT EXISTS oidc_logout_events_sub_idx ON oidc_logout_events (issuer, sub);

CREATE TABLE IF NOT EXISTS oidc_logout_states (
  id UUID PRIMARY KEY,
  handoff_hash BYTEA NOT NULL UNIQUE CHECK (octet_length(handoff_hash) = 32),
  state_hash BYTEA NOT NULL UNIQUE CHECK (octet_length(state_hash) = 32),
  id_token_encrypted TEXT NOT NULL,
  endpoint TEXT NOT NULL CHECK (endpoint <> ''),
  client_id TEXT NOT NULL CHECK (client_id <> ''),
  handoff_expires_at TIMESTAMPTZ NOT NULL,
  state_expires_at TIMESTAMPTZ NOT NULL,
  handoff_consumed_at TIMESTAMPTZ NULL,
  state_consumed_at TIMESTAMPTZ NULL,
  CHECK (handoff_expires_at < state_expires_at)
);
CREATE INDEX IF NOT EXISTS oidc_logout_states_expires_idx ON oidc_logout_states (state_expires_at);


CREATE TABLE IF NOT EXISTS groups (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  owner__id UUID NOT NULL REFERENCES users (id),
  currency_mode TEXT NOT NULL DEFAULT 'single' CHECK (currency_mode IN ('single', 'multi')),
  default_currency TEXT NOT NULL DEFAULT 'USD' CHECK (default_currency ~ '^[A-Z]{3}$'),
  currency_locked BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS group_members (
  user__id UUID REFERENCES users (id) ON DELETE CASCADE,
  group__id UUID REFERENCES groups (id) ON DELETE CASCADE,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  PRIMARY KEY (user__id, group__id)
);

CREATE TABLE IF NOT EXISTS receipts (
  id UUID PRIMARY KEY,
  group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE,
  uploaded_by__id UUID NOT NULL REFERENCES users (id),
  note TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE IF NOT EXISTS exchange_quotes (
  id UUID PRIMARY KEY,
  group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE,
  source_currency TEXT NOT NULL CHECK (source_currency ~ '^[A-Z]{3}$'),
  target_currency TEXT NOT NULL CHECK (target_currency ~ '^[A-Z]{3}$'),
  rate NUMERIC(36,18) NOT NULL CHECK (rate > 0 AND rate <> 'NaN'::numeric),
  provider TEXT NOT NULL CHECK (provider = 'frankfurter'),
  rate_date DATE NOT NULL,
  fetched_at TIMESTAMPTZ NOT NULL,
  CHECK (source_currency <> target_currency)
);

CREATE TABLE IF NOT EXISTS records (
  id UUID PRIMARY KEY,
  group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE,
  record_kind TEXT NOT NULL,
  title TEXT NOT NULL,
  note TEXT,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  settlement_currency TEXT NOT NULL CHECK (settlement_currency ~ '^[A-Z]{3}$'),
  settlement_amount_minor BIGINT NOT NULL CHECK (settlement_amount_minor > 0),
  split_mode TEXT NULL,
  exchange_rate NUMERIC(36,18) NULL,
  rate_source TEXT NULL,
  rate_date DATE NULL,
  rate_fetched_at TIMESTAMPTZ NULL,
  exchange_quote__id UUID NULL REFERENCES exchange_quotes (id),
  by__id UUID NOT NULL REFERENCES users (id),
  to__id UUID REFERENCES users (id),
  date DATE NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  receipt__id UUID REFERENCES receipts (id) ON DELETE SET NULL,
  CONSTRAINT records_kind_shape CHECK (((record_kind IN ('expense', 'income')
             AND to__id IS NULL AND split_mode IS NOT NULL
             AND split_mode IN ('equal', 'shares', 'percentage', 'exact'))
          OR (record_kind = 'transfer' AND to__id IS NOT NULL
              AND split_mode IS NULL AND exchange_rate IS NULL)) IS TRUE),
  CHECK (((exchange_rate IS NULL AND rate_source IS NULL AND rate_date IS NULL
             AND rate_fetched_at IS NULL AND exchange_quote__id IS NULL
             AND currency = settlement_currency AND amount_minor = settlement_amount_minor)
          OR (currency <> settlement_currency AND exchange_rate > 0
             AND exchange_rate <> 'NaN'::numeric
             AND ((rate_source = 'reference' AND exchange_quote__id IS NOT NULL
                   AND rate_date IS NOT NULL AND rate_fetched_at IS NOT NULL)
                  OR (rate_source = 'manual' AND exchange_quote__id IS NULL
                      AND rate_date IS NULL AND rate_fetched_at IS NULL)))) IS TRUE)
);

ALTER TABLE records ADD COLUMN IF NOT EXISTS note TEXT;

CREATE INDEX IF NOT EXISTS records_group_date_idx ON records (group__id, date);

CREATE TABLE IF NOT EXISTS record_splits (
  record__id UUID REFERENCES records (id) ON DELETE CASCADE,
  user__id UUID REFERENCES users (id),
  split_value BIGINT NOT NULL CHECK (split_value >= 0),
  amount_minor BIGINT NOT NULL CHECK (amount_minor >= 0),
  settlement_amount_minor BIGINT NOT NULL CHECK (settlement_amount_minor >= 0),
  PRIMARY KEY (record__id, user__id)
);
