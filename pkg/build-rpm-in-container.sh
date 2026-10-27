#!/bin/bash
#
# Builds the rpki-prover .rpm. Runs *inside* the container produced by
# pkg/Dockerfile.rpm-builder; pkg/build-rpm-packages.sh is the host side of
# this, and pkg/build-deb-in-container.sh its .deb counterpart.
#
# Expects:
#   /src   read-only checkout of the repository
#   /out   directory the resulting packages are copied into
#
# Configured through the environment (see pkg/build-rpm-packages.sh):
#   RELEASE         release name from pkg/rpm-releases.conf, e.g. el9  (required)
#   RPM_REVISION    the Release: number before %{dist}                (default: 1)
#   MAINTAINER      changelog maintainer
#   HOST_UID/HOST_GID  ownership to restore on /out afterwards

set -euo pipefail

SRC_DIR=${SRC_DIR:-/src}
BUILD_ROOT=${BUILD_ROOT:-/build}
OUT_DIR=${OUT_DIR:-/out}

: "${RELEASE:?RELEASE must be set}"
RPM_REVISION=${RPM_REVISION:-1}
MAINTAINER=${MAINTAINER:-Mikhail Puzanov <misha.puzanov@pm.me>}

log() { printf '\n=== %s\n' "$*" >&2; }

# ---------------------------------------------------------------------------
# 1. Copy the sources out of the read-only mount
# ---------------------------------------------------------------------------
SRC_TREE="${BUILD_ROOT}/rpki-prover"
log "Copying sources to ${SRC_TREE}"
rm -rf "${SRC_TREE}"
mkdir -p "${SRC_TREE}"
# .git is copied along: write-build-info.sh derives the build's git metadata
# from it, and without a repository it would record "unknown".
rsync -a \
    --exclude '/.stack-work/' \
    --exclude '/dist-newstyle/' \
    --exclude '/.vagrant/' \
    --exclude '/pkg/packages/' \
    --exclude '/pkg/apt-repo/public/' \
    --exclude '/debian/' \
    --exclude '/releases/' \
    --exclude '/stage/' \
    "${SRC_DIR}/" "${SRC_TREE}/"

cd "${SRC_TREE}"

# ---------------------------------------------------------------------------
# 2. Record the git metadata
# ---------------------------------------------------------------------------
log "Preparing sources"
# cabal install builds a copy of the sdist in /tmp, where there is no git
# repository to ask; see build-info/README.txt.
git config --global --add safe.directory "${SRC_TREE}" 2>/dev/null || true
bash ./write-build-info.sh

UPSTREAM_VERSION=$(awk '/^version:/ { print $2; exit }' rpki-prover.cabal)
if [ -z "${UPSTREAM_VERSION}" ]; then
    echo "Could not determine upstream version from rpki-prover.cabal" >&2
    exit 1
fi
DIST=$(rpm --eval '%{?dist}')

# ---------------------------------------------------------------------------
# 3. Stamp the changelog entry into the spec
# ---------------------------------------------------------------------------
SPEC="${BUILD_ROOT}/rpki-prover.spec"
cp pkg/rpm/rpki-prover.spec "${SPEC}"
cat >> "${SPEC}" <<EOF
* $(LC_ALL=C date '+%a %b %d %Y') ${MAINTAINER} - ${UPSTREAM_VERSION}-${RPM_REVISION}${DIST}
- rpki-prover ${UPSTREAM_VERSION} built for ${RELEASE}.
  Upstream release notes: https://github.com/lolepezy/rpki-prover/releases
EOF

# ---------------------------------------------------------------------------
# 4. Build
# ---------------------------------------------------------------------------
log "Building rpki-prover ${UPSTREAM_VERSION}-${RPM_REVISION}${DIST} for ${RELEASE}"
TOPDIR="${BUILD_ROOT}/rpmbuild"
rm -rf "${TOPDIR}"
rpmbuild -bb --build-in-place \
    --define "_topdir ${TOPDIR}" \
    --define "upstream_version ${UPSTREAM_VERSION}" \
    --define "rpm_release ${RPM_REVISION}" \
    "${SPEC}"

# ---------------------------------------------------------------------------
# 5. Collect and check
# ---------------------------------------------------------------------------
log "Collecting artifacts"
mkdir -p "${OUT_DIR}"
find "${TOPDIR}/RPMS" -name '*.rpm' -exec cp -v {} "${OUT_DIR}/" \;

for rpm in "${TOPDIR}"/RPMS/*/*.rpm; do
    log "$(basename "${rpm}")"
    rpm -qpi "${rpm}"
    echo "--- requires"
    rpm -qp --requires "${rpm}"
    echo "--- files"
    rpm -qplv "${rpm}"
done

if command -v rpmlint >/dev/null; then
    log "rpmlint (informational, does not fail the build)"
    rpmlint "${TOPDIR}"/RPMS/*/*.rpm || true
fi

if [ -n "${HOST_UID:-}" ] && [ -n "${HOST_GID:-}" ]; then
    chown -R "${HOST_UID}:${HOST_GID}" "${OUT_DIR}"
fi

log "Done: $(cd "${OUT_DIR}" && ls -1 *.rpm | tr '\n' ' ')"
