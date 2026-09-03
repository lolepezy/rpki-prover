#!/bin/sh
# Provisioning for the OpenBSD Vagrant VM: installs build-time and
# runtime dependencies via pkg_add. Runs as root (Vagrant's default for
# shell provisioners).
#
# Note: pkg_add can exit 0 even when it found none of the requested
# packages (e.g. every package missing from the mirror), so this verifies
# what actually got installed afterward instead of trusting the exit code.
set -eu

echo "== pkg_add: installing rpki-prover build/runtime dependencies =="

# pkg_add uses the mirror configured in /etc/installurl, which stock
# OpenBSD images set up automatically. "no such dir" here almost always
# means the release is too old: OpenBSD only keeps packages live for the
# current + previous release. See DEVELOPER.md ("OpenBSD") and the
# Vagrantfile header for the sysupgrade recipe.
pkg_add -Iz ghc git gmake lmdb xz gmp rsync || true

# hs-cabal-install has been renamed/replaced before; try a few candidates
# rather than hardcoding one that might be wrong for this release.
cabal_installed=0
for pkg in hs-cabal-install cabal-install cabal; do
    if command -v cabal >/dev/null 2>&1; then
        cabal_installed=1
        break
    fi
    pkg_add -Iz "$pkg" 2>/dev/null || true
done
command -v cabal >/dev/null 2>&1 && cabal_installed=1

missing=""
for bin in ghc cabal git gmake rsync; do
    command -v "$bin" >/dev/null 2>&1 || missing="$missing $bin"
done
for lib in lmdb xz gmp; do
    pkg_info -e "${lib}-*" >/dev/null 2>&1 || missing="$missing $lib"
done

if [ -n "$missing" ]; then
    cat >&2 <<EOF

Provisioning FAILED, still missing:$missing

This usually means either the release has no live packages (see the
Vagrantfile header for the sysupgrade fix), or a package name above is
wrong for this OpenBSD release. Check current names with, e.g.:
    pkg_info -Q ghc
    pkg_info -Q cabal
EOF
    exit 1
fi

echo "== versions =="
ghc --version
cabal --version
rsync --version | head -1

echo
echo "Provisioning done. Build with:"
echo "    vagrant ssh -c 'cd /vagrant && ./build-openbsd.sh'"
