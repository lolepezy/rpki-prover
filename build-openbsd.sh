#!/bin/sh
# Build rpki-prover on OpenBSD.
#
# GHC is not distributed via ghcup on OpenBSD (ghcup ships no OpenBSD
# bindists), so GHC/cabal-install must come from pkg_add instead. Package
# names occasionally drift between OpenBSD releases, so this script checks
# for the *binaries* it actually needs rather than hardcoding pkg_add names.
#
# See DEVELOPER.md ("OpenBSD") for the full explanation and caveats
# (GHC version mismatch against cabal.project, rsync flag compatibility).
set -eu

if [ "$(uname -s)" != "OpenBSD" ]; then
    echo "Warning: this doesn't look like OpenBSD (uname -s = $(uname -s))." >&2
    echo "Continuing anyway, but you probably want build-local.sh instead." >&2
fi

missing=""
for bin in ghc cabal git gmake; do
    command -v "$bin" >/dev/null 2>&1 || missing="$missing $bin"
done

if [ -n "$missing" ]; then
    cat >&2 <<EOF
Missing required tool(s):$missing

Install them with pkg_add, e.g.:
    doas pkg_add ghc hs-cabal-install git gmake lmdb xz gmp

Package names can differ between OpenBSD releases; if the above fails, find
the right ones with:
    pkg_info -Q ghc
    pkg_info -Q cabal
EOF
    exit 1
fi

echo "Using: $(ghc --version), $(cabal --version)"
echo "Note: this is whatever GHC/cabal pkg_add installed, which may not match"
echo "the ghc-9.10.3 pinned in cabal.project. If cabal complains about the"
echo "compiler version, override it locally without touching cabal.project:"
echo "    echo 'with-compiler: ghc-<installed-version>' > cabal.project.local"
echo

. ./generate-modules.sh

cabal -j4 install rpki-prover:exe:rpki-prover --overwrite-policy=always

cat <<'EOF'

Build finished. Before running rpki-prover:

- Runtime needs an rsync client. OpenBSD's base rsync is openrsync, which
  supports a smaller flag set than GNU rsync (this project uses --contimeout,
  --max-size, --min-size and --copy-links). If validation runs fail rsync
  fetches, install GNU rsync from ports and point rpki-prover at it:
      doas pkg_add rsync
      rpki-prover --rsync-client-path=/usr/local/bin/rsync ...
EOF
