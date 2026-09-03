#!/bin/sh
# Build and package an rpki-prover release tarball for OpenBSD.
#
# Unlike the Linux release binary (build-static.sh), this is NOT statically
# linked: OpenBSD ports generally don't ship static archives (.a) for
# anything beyond base, and OpenBSD's security model doesn't expect static
# binaries the way musl/Alpine does on Linux. So this produces a normal
# dynamically-linked binary; the tarball's README notes which pkg_add
# packages are needed at runtime.
#
# Must be run ON an OpenBSD host/VM (see Vagrantfile) -- there is no
# cross-compilation path to OpenBSD from Linux/macOS.
set -eu

if [ "$(uname -s)" != "OpenBSD" ]; then
    echo "This must be run on OpenBSD (uname -s = $(uname -s))." >&2
    exit 1
fi

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
cd "$SCRIPT_DIR"

VERSION="$(awk '/^version:/ {print $2}' package.yaml)"
ARCH="$(uname -m)"
PKG_NAME="rpki-prover-${VERSION}-openbsd-${ARCH}"
DIST_DIR="dist/${PKG_NAME}"

echo "Building rpki-prover ${VERSION} for OpenBSD/${ARCH}..."
./build-openbsd.sh

BUILT_BIN="$(cabal list-bin rpki-prover:exe:rpki-prover)"

rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR"
cp "$BUILT_BIN" "$DIST_DIR/rpki-prover"
cp README.md LICENSE ChangeLog.md "$DIST_DIR/"

cat > "$DIST_DIR/OPENBSD-README.txt" <<EOF
rpki-prover ${VERSION} -- OpenBSD/${ARCH} build

This is a dynamically linked binary, built on $(uname -srm).
Runtime dependencies (install via pkg_add on the target machine if not
already present): lmdb, xz, gmp.

rpki-prover also needs an rsync client at runtime. OpenBSD's base openrsync
supports a smaller flag set than GNU rsync; if repository fetches fail,
install GNU rsync from ports and pass --rsync-client-path:
    doas pkg_add rsync
    rpki-prover --rsync-client-path=/usr/local/bin/rsync ...

Run 'rpki-prover --help' for CLI options.
EOF

TARBALL="${PKG_NAME}.tar.gz"
tar -czf "dist/${TARBALL}" -C dist "${PKG_NAME}"

echo
echo "Packaged: dist/${TARBALL}"
echo "Upload this to the GitHub release alongside the Linux static binary."
