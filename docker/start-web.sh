#!/bin/sh
set -eu

# This is public browser configuration, never a secret or shell program.
LC_ALL=C
export LC_ALL
value=${API_BASE_URL-}
invalid() {
    printf '%s\n' 'API_BASE_URL must be an HTTP(S) base URL without credentials, query or fragment' >&2
    exit 1
}
if [ -n "$value" ]; then
    case "$value" in
        http://*|https://*) ;;
        *) invalid ;;
    esac
    case "$value" in
        *'@'*|*'?'*|*'#'*|*'\'*|*[[:space:][:cntrl:]]*) invalid ;;
    esac
    authority=${value#*://}
    authority=${authority%%/*}
    [ -n "$authority" ] || invalid
fi

# A missing/unwritable runtime mount fails before the server can listen.
temporary=$(mktemp /public/runtime/.api-base-url.XXXXXX)
trap 'rm -f "$temporary"' EXIT HUP INT TERM
printf '%s\n' "$value" > "$temporary"
chmod 0644 "$temporary"
mv -f "$temporary" /public/runtime/api-base-url.txt
trap - EXIT HUP INT TERM
exec /usr/local/bin/static-web-server --config-file /etc/jizhang-web.toml
