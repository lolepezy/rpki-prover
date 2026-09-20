#!/bin/bash
#
# Builds the rpki-prover .deb. Runs *inside* the container produced by
# pkg/Dockerfile.deb-builder; pkg/build-packages.sh is the host side of this.
#
# Expects:
#   /src   read-only checkout of the repository
#   /out   directory the resulting artifacts are copied into
#
# Configured through the environment (see pkg/build-packages.sh):
#   CODENAME        debian release codename, e.g. bookworm       (required)
#   DISTRIBUTION    changelog distribution, e.g. unstable        (default: $CODENAME)
#   VERSION_SUFFIX  appended to the debian version, e.g. ~deb12u1
#   DEB_REVISION    debian revision                              (default: 1)
#   MAINTAINER      changelog maintainer
#   HOST_UID/HOST_GID  ownership to restore on /out afterwards

set -euo pipefail

SRC_DIR=${SRC_DIR:-/src}
BUILD_ROOT=${BUILD_ROOT:-/build}
OUT_DIR=${OUT_DIR:-/out}

: "${CODENAME:?CODENAME must be set}"
DISTRIBUTION=${DISTRIBUTION:-${CODENAME}}
VERSION_SUFFIX=${VERSION_SUFFIX:-}
DEB_REVISION=${DEB_REVISION:-1}
MAINTAINER=${MAINTAINER:-Mikhail Puzanov <misha.puzanov@pm.me>}

log() { printf '\n=== %s\n' "$*" >&2; }

# ---------------------------------------------------------------------------
# 1. Copy the sources out of the read-only mount
# ---------------------------------------------------------------------------
SRC_TREE="${BUILD_ROOT}/rpki-prover"
log "Copying sources to ${SRC_TREE}"
rm -rf "${SRC_TREE}"
mkdir -p "${SRC_TREE}"
# .git is copied along: generate-modules.sh derives the build's git metadata
# from it, and without a repository it would bake in "unknown".
rsync -a \
    --exclude '/.stack-work/' \
    --exclude '/dist-newstyle/' \
    --exclude '/.vagrant/' \
    --exclude '/pkg/packages/' \
    --exclude '/pkg/apt-repo/public/' \
    --exclude '/debian/' \
    "${SRC_DIR}/" "${SRC_TREE}/"

cd "${SRC_TREE}"

# ---------------------------------------------------------------------------
# 2. Generate the files the normal build scripts generate
# ---------------------------------------------------------------------------
log "Preparing sources"
# Generates src/RPKI/Meta/GitVersionInfo.hs and stamps the source-tree hash
# into src/RPKI/Meta/UniqueId.hs; rpki-prover uses both at runtime to tell
# whether workers and parent come from the same build.
git config --global --add safe.directory "${SRC_TREE}" 2>/dev/null || true
bash ./generate-modules.sh

UPSTREAM_VERSION=$(awk '/^version:/ { print $2; exit }' rpki-prover.cabal)
if [ -z "${UPSTREAM_VERSION}" ]; then
    echo "Could not determine upstream version from rpki-prover.cabal" >&2
    exit 1
fi
DEB_VERSION="${UPSTREAM_VERSION}-${DEB_REVISION}${VERSION_SUFFIX}"

# ---------------------------------------------------------------------------
# 3. Materialise debian/ from the templates in pkg/debian
# ---------------------------------------------------------------------------
log "Building rpki-prover ${DEB_VERSION} for ${CODENAME}"
rm -rf debian
cp -a pkg/debian debian
rm -f debian/changelog.in

sed -e "s|@DEB_VERSION@|${DEB_VERSION}|g" \
    -e "s|@UPSTREAM_VERSION@|${UPSTREAM_VERSION}|g" \
    -e "s|@DISTRIBUTION@|${DISTRIBUTION}|g" \
    -e "s|@CODENAME@|${CODENAME}|g" \
    -e "s|@MAINTAINER@|${MAINTAINER}|g" \
    -e "s|@DATE@|$(date -R)|g" \
    pkg/debian/changelog.in > debian/changelog

chmod +x debian/rules debian/rpki-prover.postinst debian/rpki-prover.postrm

# ---------------------------------------------------------------------------
# 4. Build
# ---------------------------------------------------------------------------
dpkg-buildpackage --build=binary --no-sign

# ---------------------------------------------------------------------------
# 5. Collect and check
# ---------------------------------------------------------------------------
log "Collecting artifacts"
mkdir -p "${OUT_DIR}"
cp -v "${BUILD_ROOT}"/*.deb "${OUT_DIR}/"
cp -v "${BUILD_ROOT}"/*.buildinfo "${BUILD_ROOT}"/*.changes "${OUT_DIR}/" 2>/dev/null || true

log "lintian (informational, does not fail the build)"
lintian --tag-display-limit 0 --info "${BUILD_ROOT}"/*.changes || true

if [ -n "${HOST_UID:-}" ] && [ -n "${HOST_GID:-}" ]; then
    chown -R "${HOST_UID}:${HOST_GID}" "${OUT_DIR}"
fi

log "Done: $(cd "${OUT_DIR}" && ls -1 *.deb | tr '\n' ' ')"
