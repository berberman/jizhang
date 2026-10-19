#!/bin/sh
set -eu

case "${APP_DB_USER:-}" in
  ''|postgres)
    printf '%s\n' 'APP_DB_USER must be nonempty and must not be postgres' >&2
    exit 1
    ;;
esac
case "${APP_DB_NAME:-}" in
  ''|postgres|template0|template1)
    printf '%s\n' 'APP_DB_NAME must be nonempty and must not be postgres, template0 or template1' >&2
    exit 1
    ;;
esac
if [ -z "${APP_DB_PASSWORD:-}" ]; then
  printf '%s\n' 'APP_DB_PASSWORD must be nonempty' >&2
  exit 1
fi

# psql quotes identifiers/literals; the shell never expands SQL. Disable statement
# diagnostics before password-bearing DDL, including on a failed CREATE ROLE.
psql --no-psqlrc --set ON_ERROR_STOP=1 --set ECHO=none --set VERBOSITY=terse \
  --username "$POSTGRES_USER" --dbname postgres <<'SQL'
SET log_statement = 'none';
SET log_min_error_statement = 'panic';
\getenv app_user APP_DB_USER
\getenv app_database APP_DB_NAME
\getenv app_password APP_DB_PASSWORD
CREATE ROLE :"app_user" LOGIN PASSWORD :'app_password' NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION;
CREATE DATABASE :"app_database" OWNER :"app_user";
SQL
