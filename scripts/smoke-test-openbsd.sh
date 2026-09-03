#!/bin/sh
# Minimal smoke test for a built rpki-prover binary on OpenBSD.
#
# Deliberately does NOT run a real validation pass (needs network access to
# RPKI repositories and takes minutes), just checks the binary starts,
# links correctly (dynamic loader can resolve lmdb/lzma/etc.), and parses
# its own CLI.
set -eu

BIN="${1:-./rpki-prover}"

if [ ! -x "$BIN" ]; then
    echo "smoke test: $BIN not found or not executable" >&2
    exit 1
fi

echo "smoke test: $BIN --version"
"$BIN" --version

echo "smoke test: $BIN --help"
"$BIN" --help >/dev/null

echo "smoke test: rsync client check"
if command -v rsync >/dev/null 2>&1; then
    rsync --version | head -1
else
    echo "warning: no rsync in PATH; rpki-prover needs one at runtime" >&2
fi

echo "smoke test: OK"
