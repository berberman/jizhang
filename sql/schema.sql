CREATE TABLE IF NOT EXISTS users (
  id UUID PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS admins (
  id UUID PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS groups (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  owner__id UUID NOT NULL REFERENCES users (id)
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

CREATE TABLE IF NOT EXISTS records (
  id UUID PRIMARY KEY,
  group__id UUID NOT NULL REFERENCES groups (id) ON DELETE CASCADE,
  title TEXT NOT NULL,
  amount DOUBLE PRECISION NOT NULL,
  by__id UUID NOT NULL REFERENCES users (id),
  to__id UUID REFERENCES users (id),
  date DATE NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  receipt__id UUID REFERENCES receipts (id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS record_splits (
  record__id UUID REFERENCES records (id) ON DELETE CASCADE,
  user__id UUID REFERENCES users (id),
  share SMALLINT NOT NULL CHECK (share >= 0),
  PRIMARY KEY (record__id, user__id)
);
