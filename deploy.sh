#!/usr/bin/env bash
set -euo pipefail

BASE_ROOT="${1:?Usage: deploy.sh <root-directory> [--reset]}"
BASE_ROOT="$(realpath "$BASE_ROOT")"

RESET=0
for arg in "${@:2}"; do
    [ "$arg" = "--reset" ] && RESET=1
done

BRANCH="$(git rev-parse --abbrev-ref HEAD)"
BRANCH_SAFE="${BRANCH//\//-}"          # / → - so it's a valid directory name
ROOT="$BASE_ROOT/$BRANCH_SAFE"
mkdir -p "$ROOT"

# Deterministic port in [10000, 59999] derived from the absolute ROOT path.
# Different branches → different ROOT paths → different ports, no manual bookkeeping needed.
PORT=$(( 10000 + $(printf '%s' "$ROOT" | cksum | awk '{print $1}') % 50000 ))

PID_FILE="$ROOT/rpki-prover.pid"

# Kill the previous instance for this branch, if any.
if [ -f "$PID_FILE" ]; then
    OLD_PID="$(cat "$PID_FILE")"
    if kill -0 "$OLD_PID" 2>/dev/null; then
        [ -f "$ROOT/log" ] && mv "$ROOT/log" "$ROOT/log.$(date +%Y%m%d_%H%M%S)"
        kill "$OLD_PID"
        # Wait up to 15 s for a clean shutdown before proceeding.
        for i in $(seq 1 30); do
            kill -0 "$OLD_PID" 2>/dev/null || break
            sleep 0.5
        done
    fi
    rm -f "$PID_FILE"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
(cd "$SCRIPT_DIR" && ./build-local.sh)

cp "$HOME/.cabal/bin/rpki-prover" "$ROOT/rpki-prover.new"
mv "$ROOT/rpki-prover.new" "$ROOT/rpki-prover"

cd "$ROOT"
RESET_FLAG=""
[ "$RESET" = "1" ] && RESET_FLAG="--reset-cache"

./rpki-prover \
    --rpki-root-directory "$ROOT" \
    --cpu-count 8 \
    --log-level debug \
    --http-api-port "$PORT" \
    --allow-overclaiming \
    --show-hidden-config \
    $RESET_FLAG \
    > "$ROOT/log" 2>&1 &

echo $! > "$PID_FILE"

echo "Branch : $BRANCH"
echo "Root   : $ROOT"
echo "Port   : $PORT"
echo "PID    : $(cat "$PID_FILE")"
echo "Log    : $ROOT/log"
