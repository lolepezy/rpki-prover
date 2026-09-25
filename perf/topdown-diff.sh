#!/usr/bin/env bash
#
# Compare what two builds of the top-down validation produce from the same
# cache: payloads, issues, metrics, the objects marked as used and the
# manifest shortcuts written. A change that isn't meant to change behaviour
# should come out with no differences at all.
#
# Usage:
#   perf/topdown-diff.sh SOURCE_ROOT WORK_DIR BASE_BIN NEW_BIN [SCENARIO...]
#
#   SOURCE_ROOT  root of an rpki-prover instance (cache/rpki.sqlite and tals/),
#                of the same database version as the two builds
#   WORK_DIR     where the copy of the cache, the runs and their dumps go; it
#                needs a bit more than twice the size of the cache
#   BASE_BIN, NEW_BIN
#                two builds of validate-tas-bench, e.g.
#                  cabal build validate-tas-bench --enable-benchmarks
#                  cp "$(cabal list-bin validate-tas-bench --enable-benchmarks)" WORK_DIR/base
#
# The cache is copied once, with SQLite's backup API, into WORK_DIR/source.sqlite
# and its time is kept in WORK_DIR/now. Every run starts from that copy and
# validates as of that time, so delete both to start from a fresh copy.
#
# Scenarios (all of them by default):
#   live         the cache as it is, 2 rounds: mostly unchanged manifests, plus
#                the ones fetched since the instance last validated
#   cold         no shortcuts, 2 rounds: everything in full, then from shortcuts
#   later        12 hours later, 2 rounds: expired manifests and shortcuts
#   full         --no-incremental-validation, 1 round
#   cold-strict  cold, with strict RFC resource checks and strict manifests
#
# All but cold-strict run with --allow-overclaiming, which is what the
# instances under ~/tmp/rpki run with, so the shortcuts they made are
# rechecked the way they were made.
#
# Running the same binary as both builds shows what differs between runs
# anyway, e.g. because of the order things happen in.

set -euo pipefail

if [ $# -lt 4 ]; then
    sed -n '3,40p' "$0" | sed 's/^# \{0,1\}//'
    exit 1
fi

SOURCE_ROOT=$(realpath "$1")
WORK_DIR=$(realpath -m "$2")
BASE_BIN=$(realpath "$3")
NEW_BIN=$(realpath "$4")
shift 4
if [ $# -gt 0 ]; then
    SCENARIOS=("$@")
else
    SCENARIOS=(live cold later full cold-strict)
fi

mkdir -p "$WORK_DIR"
SOURCE_DB="$WORK_DIR/source.sqlite"

if [ ! -f "$SOURCE_DB" ] || [ ! -f "$WORK_DIR/now" ]; then
    echo "Copying $SOURCE_ROOT/cache/rpki.sqlite to $SOURCE_DB"
    rm -f "$SOURCE_DB" "$WORK_DIR/now"
    date +%s > "$WORK_DIR/now.tmp"
    python3 - "$SOURCE_ROOT/cache/rpki.sqlite" "$SOURCE_DB" <<'EOF'
import sqlite3, sys
src = sqlite3.connect(f'file:{sys.argv[1]}?mode=ro', uri=True)
dst = sqlite3.connect(sys.argv[2])
src.backup(dst)
dst.close()
EOF
    mv "$WORK_DIR/now.tmp" "$WORK_DIR/now"
fi
NOW=$(cat "$WORK_DIR/now")

# The manifest shortcuts as rows, the blobs as their hashes
dump_shortcuts() {
    python3 - "$1" "$2" <<'EOF'
import hashlib, sqlite3, sys
db, out = sys.argv[1], sys.argv[2]
c = sqlite3.connect(f'file:{db}?mode=ro', uri=True)
def h(b):  return hashlib.sha1(b).hexdigest()
def k(v):  return v.hex() if isinstance(v, bytes) else str(v)
def dump(name, query, fmt):
    with open(f'{out}/{name}', 'w') as f:
        for row in c.execute(query):
            f.write('\t'.join(fmt(row)) + '\n')
dump('shortcut-meta.txt',
     'SELECT aki, data FROM mft_shortcut_meta ORDER BY aki',
     lambda r: (k(r[0]), h(r[1])))
dump('shortcut-children.txt',
     'SELECT aki, child_key, file_name FROM mft_shortcut_children ORDER BY aki, child_key',
     lambda r: (k(r[0]), str(r[1]), r[2]))
dump('shortcuts.txt',
     'SELECT object_key, data FROM shortcuts ORDER BY object_key',
     lambda r: (str(r[0]), h(r[1])))
EOF
}

wipe_shortcuts() {
    python3 - "$1" <<'EOF'
import sqlite3, sys
c = sqlite3.connect(sys.argv[1])
c.executescript('''
    DELETE FROM mft_shortcut_children;
    DELETE FROM shortcuts;
    DELETE FROM mft_shortcut_meta;
''')
c.close()
EOF
}

# run_scenario SCENARIO BUILD BIN
run_scenario() {
    local scenario=$1 build=$2 bin=$3
    local rounds=2 offset=0 flags=(--reconsidered) cold=no
    case "$scenario" in
        live)        ;;
        cold)        cold=yes ;;
        later)       offset=$((12 * 3600)) ;;
        full)        rounds=1; flags+=(--full) ;;
        cold-strict) cold=yes; flags=(--strict-manifests) ;;
        *)           echo "Unknown scenario $scenario"; exit 1 ;;
    esac

    local root="$WORK_DIR/run/$scenario-$build"
    local out="$WORK_DIR/out/$scenario/$build"
    rm -rf "$root" "$out"
    mkdir -p "$root/cache" "$out"
    cp -r "$SOURCE_ROOT/tals" "$root/tals"
    cp "$SOURCE_DB" "$root/cache/rpki.sqlite"
    if [ "$cold" = yes ]; then
        wipe_shortcuts "$root/cache/rpki.sqlite"
    fi

    for round in $(seq 1 "$rounds"); do
        # Every round is a world version of its own, a second apart
        local now=$((NOW + offset + round - 1))
        local dump="$out/round$round"
        "$bin" "$root" 1 --now="$now" --dump="$dump" --debug "${flags[@]}" \
            > "$out/round$round.log" 2>&1 \
            || { echo "  $build round $round failed, see $out/round$round.log"; return 1; }
        dump_shortcuts "$root/cache/rpki.sqlite" "$dump"
        printf "  %-5s round %d: %s\n" "$build" "$round" \
            "$(grep -o 'wall=[0-9.]*s cpu=[0-9.]*s' "$out/round$round.log" | head -1)"
        # Which paths the validation took, e.g. how many manifests from shortcuts
        grep -o 'TopDownCounters {.*}' "$out/round$round.log" | head -1 \
            | sed -e 's/TopDownCounters {//' -e 's/}$//' -e 's/ = /=/g' -e 's/^/      /'
    done
    rm -rf "$root"
}

different=()
for scenario in "${SCENARIOS[@]}"; do
    echo "== $scenario"
    run_scenario "$scenario" base "$BASE_BIN"
    run_scenario "$scenario" new  "$NEW_BIN"

    report="$WORK_DIR/out/$scenario/diff.txt"
    if diff -r "$WORK_DIR/out/$scenario/base" "$WORK_DIR/out/$scenario/new" \
            -x '*.log' > "$report"; then
        echo "  no differences"
    else
        different+=("$scenario")
        echo "  DIFFERENT, $(grep -c '^[<>]' "$report") lines, see $report:"
        diff -rq "$WORK_DIR/out/$scenario/base" "$WORK_DIR/out/$scenario/new" -x '*.log' \
            | sed 's/^/    /' || true
    fi
done

if [ ${#different[@]} -eq 0 ]; then
    echo "No differences"
else
    echo "Differences in: ${different[*]}"
    exit 2
fi
