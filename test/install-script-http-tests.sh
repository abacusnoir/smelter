#!/usr/bin/env bash
set -euo pipefail

# Ensure we run from repo root
cd "$(dirname "$0")/.."

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

STUB_DIR="$TMP_DIR/stub"
mkdir -p "$STUB_DIR"

CURL_LOG="$TMP_DIR/curl_args.log"
: > "$CURL_LOG"

cat <<'STUB' > "$STUB_DIR/curl"
#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${SMELTER_TEST_CURL_LOG:-}" ]]; then
    echo "SMELTER_TEST_CURL_LOG is not set" >&2
    exit 1
fi

for arg in "$@"; do
    printf '%s\n' "$arg" >> "$SMELTER_TEST_CURL_LOG"

done

if [[ "$*" == *"https://example.com/success"* ]]; then
    if [[ "$*" == *" -o "* ]]; then
        # handle download requests
        while [[ $# -gt 0 ]]; do
            if [[ "$1" == "-o" ]]; then
                shift
                dest="$1"
                printf 'dummy binary' > "$dest"
                exit 0
            fi
            shift
        done
        echo "curl stub did not receive -o destination" >&2
        exit 1
    else
        printf 'ok'
        exit 0
    fi
elif [[ "$*" == *"https://example.com/fail"* ]]; then
    exit 22
else
    printf 'unhandled url: %s\n' "$*" >&2
    exit 1
fi
STUB
chmod +x "$STUB_DIR/curl"

# Make sure our stub curl is picked before the real one.
export PATH="$STUB_DIR:$PATH"
export SMELTER_TEST_CURL_LOG="$CURL_LOG"

# Provide token before sourcing installer so AUTH_HEADER is set.
export GITHUB_TOKEN="secret-token"
# Force deterministic retry counts for assertions.
export HTTP_RETRIES=3
export HTTP_RETRY_DELAY=2

# Source the installer to access helper functions.
# shellcheck disable=SC1091
source scripts/install.sh

HTTP_CLIENT="curl"

# http_request should succeed and emit expected payload.
request_output=$(http_request "https://example.com/success")
if [[ "$request_output" != "ok" ]]; then
    echo "http_request did not return expected payload" >&2
    exit 1
fi

# Verify critical retry and auth options were passed to curl.
if ! grep -Fx -- '--retry' "$CURL_LOG" >/dev/null; then
    echo "--retry flag missing" >&2
    exit 1
fi
if ! grep -Fx -- "$HTTP_RETRIES" "$CURL_LOG" >/dev/null; then
    echo "retry count missing" >&2
    exit 1
fi
if ! grep -Fx -- '--retry-delay' "$CURL_LOG" >/dev/null; then
    echo "--retry-delay flag missing" >&2
    exit 1
fi
if ! grep -Fx -- "$HTTP_RETRY_DELAY" "$CURL_LOG" >/dev/null; then
    echo "retry delay missing" >&2
    exit 1
fi
if ! grep -Fx -- '--retry-all-errors' "$CURL_LOG" >/dev/null; then
    echo "--retry-all-errors flag missing" >&2
    exit 1
fi
if ! grep -Fx -- '--connect-timeout' "$CURL_LOG" >/dev/null; then
    echo "--connect-timeout flag missing" >&2
    exit 1
fi
if ! grep -Fx -- '10' "$CURL_LOG" >/dev/null; then
    echo "connect timeout value missing" >&2
    exit 1
fi
if ! grep -Fx -- '-H' "$CURL_LOG" >/dev/null; then
    echo "-H flag missing" >&2
    exit 1
fi
if ! grep -Fx -- 'Authorization: token secret-token' "$CURL_LOG" >/dev/null; then
    echo "auth header missing" >&2
    exit 1
fi

# Reset log to inspect download call separately.
: > "$CURL_LOG"

# http_download should populate requested file.
download_target="$TMP_DIR/smt.tar.gz"
http_download "https://example.com/success" "$download_target"
if [[ ! -s "$download_target" ]]; then
    echo "http_download did not create target file" >&2
    exit 1
fi

if ! grep -Fx -- '-o' "$CURL_LOG" >/dev/null; then
    echo "-o flag missing for download" >&2
    exit 1
fi
if ! grep -Fx -- "$download_target" "$CURL_LOG" >/dev/null; then
    echo "download target missing from curl args" >&2
    exit 1
fi

# Reset log before testing failure path to ensure assertions only see the failing invocation.
: > "$CURL_LOG"

# Failure paths should propagate non-zero exit codes.
if http_download "https://example.com/fail" "$TMP_DIR/should_not_exist"; then
    echo "http_download unexpectedly succeeded on failure URL" >&2
    exit 1
fi

# Ensure failure call still attempted retries flag; check log.
if ! grep -Fx -- '--retry' "$CURL_LOG" >/dev/null; then
    echo "retry flag missing on failure invocation" >&2
    exit 1
fi

echo "All install script HTTP helper tests passed"
