#!/bin/bash
#
# Builds (and optionally signs and tests) the OpenBSD package of rpki-prover
# from the port in pkg/openbsd/net/rpki-prover, in the Vagrant VM defined next
# to this script.
#
#   pkg/openbsd/build-package.sh [--release 7.9] [--from-tag] [--sign KEY.sec] [--test]
#
#   --release R     OpenBSD release to build for (default: 7.9). The VM is
#                   sysupgraded to it if needed; a package only installs on
#                   the release it was built on.
#   --from-tag      build from the GitHub tarball of tag v<version>, checked
#                   against net/rpki-prover/distinfo, as the ports tree would;
#                   without a distinfo, record one (commit it after a look).
#                   By default the package is built from the committed HEAD
#                   instead; uncommitted changes are not included.
#   --sign KEY.sec  sign the package with this signify key, whose name must
#                   end in "pkg.sec"; KEY.pub is what users put in /etc/signify
#   --test          install the package in the VM and run guest/test-package.sh
#
# The package ends up in pkg/openbsd/packages/<release>/.

set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
REPO_ROOT=$(cd "${HERE}/../.." && pwd)

RELEASE=7.9
FROM_TAG=no
SIGN_KEY=""
RUN_TEST=no

while [ $# -gt 0 ]; do
    case $1 in
        --release)  RELEASE=$2; shift 2 ;;
        --from-tag) FROM_TAG=yes; shift ;;
        --sign)     SIGN_KEY=$(realpath "$2"); shift 2 ;;
        --test)     RUN_TEST=yes; shift ;;
        *) sed -n '2,/^$/s/^# \{0,1\}//p' "$0" >&2; exit 2 ;;
    esac
done

if [ -n "${SIGN_KEY}" ]; then
    case $(basename "${SIGN_KEY}") in
        *pkg.sec) ;;
        *) echo "pkg_add only trusts keys named *pkg.pub; rename ${SIGN_KEY} to match" >&2
           exit 2 ;;
    esac
fi

VERSION=$(sed -n 's/^version: *//p' "${REPO_ROOT}/rpki-prover.cabal")
OUT_DIR="${HERE}/packages/${RELEASE}"
SSH_CONFIG=$(mktemp)
WORK=$(mktemp -d)
trap 'rm -rf "${SSH_CONFIG}" "${WORK}"' EXIT

cd "${HERE}"

log() { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
vm()  { ssh -F "${SSH_CONFIG}" default "$@"; }
to_vm() { scp -q -F "${SSH_CONFIG}" "$@"; }

wait_for_vm() {
    sleep 30
    for _ in $(seq 1 120); do
        vm -o ConnectTimeout=5 true 2>/dev/null && return 0
        sleep 10
    done
    echo "The VM did not come back" >&2
    exit 1
}

log "Starting the VM"
vagrant up
vagrant ssh-config > "${SSH_CONFIG}"

# sysupgrade only ever goes one release forward, so this may take a few rounds.
while :; do
    current=$(vm uname -r)
    [ "${current}" = "${RELEASE}" ] && break
    if [ "$(printf '%s\n%s\n' "${current}" "${RELEASE}" | sort -V | head -1)" != "${current}" ]; then
        echo "The VM runs OpenBSD ${current}, newer than ${RELEASE}; vagrant destroy it first" >&2
        exit 1
    fi
    log "Upgrading the VM from OpenBSD ${current}"
    vm sudo sysupgrade -n
    vm sudo shutdown -r now || true
    wait_for_vm
    if [ "$(vm uname -r)" = "${current}" ]; then
        echo "sysupgrade from ${current} did not happen; is ${RELEASE} released yet?" >&2
        exit 1
    fi
done

log "Preparing the VM for OpenBSD ${RELEASE}"
to_vm "${HERE}"/guest/*.sh default:/tmp/
vm sudo ksh /tmp/prepare-vm.sh "${RELEASE}"

log "Copying the port"
rsync -a --delete -e "ssh -F ${SSH_CONFIG}" \
    "${HERE}/net/rpki-prover/" default:/build/upload/port/

LOCAL_DISTFILE=""
if [ "${FROM_TAG}" = no ]; then
    log "Making the source tarball from HEAD ($(git -C "${REPO_ROOT}" rev-parse --short HEAD))"
    src="${WORK}/rpki-prover-${VERSION}"
    mkdir -p "${src}"
    git -C "${REPO_ROOT}" archive HEAD | tar -x -C "${src}"
    # The committed GitVersionInfo.hs and UniqueId.hs describe whatever tree
    # generate-modules.sh last ran in; run it on this one. The private index
    # keeps git from touching the repository's own.
    (cd "${src}" &&
        export GIT_DIR="${REPO_ROOT}/.git" GIT_WORK_TREE="${src}" \
               GIT_INDEX_FILE="${WORK}/index" &&
        git read-tree HEAD &&
        bash ./generate-modules.sh)
    tar -C "${WORK}" -czf "${WORK}/rpki-prover-${VERSION}.tar.gz" "rpki-prover-${VERSION}"
    to_vm "${WORK}/rpki-prover-${VERSION}.tar.gz" default:/build/upload/
    LOCAL_DISTFILE="/build/upload/rpki-prover-${VERSION}.tar.gz"
fi

log "Building the package"
pkg_path=$(vm sudo ksh /tmp/build-port.sh "${LOCAL_DISTFILE}" | tee /dev/stderr | tail -1)

if [ -n "${SIGN_KEY}" ]; then
    log "Signing with $(basename "${SIGN_KEY}")"
    key_name=$(basename "${SIGN_KEY}")
    to_vm "${SIGN_KEY}" "default:/tmp/${key_name}"
    vm "rm -rf /build/upload/signed && mkdir /build/upload/signed &&
        pkg_sign -C -s signify2 -s /tmp/${key_name} -o /build/upload/signed \
            -S $(dirname "${pkg_path}");
        status=\$?; rm -f /tmp/${key_name}; exit \$status"
    pkg_path="/build/upload/signed/$(basename "${pkg_path}")"
fi

if [ "${FROM_TAG}" = yes ] && [ ! -f "${HERE}/net/rpki-prover/distinfo" ]; then
    to_vm default:/usr/ports/mystuff/net/rpki-prover/distinfo "${HERE}/net/rpki-prover/"
    log "Wrote net/rpki-prover/distinfo for v${VERSION}; review and commit it"
fi

mkdir -p "${OUT_DIR}"
to_vm "default:${pkg_path}" "${OUT_DIR}/"
log "Package: ${OUT_DIR}/$(basename "${pkg_path}")"

if [ "${RUN_TEST}" = yes ]; then
    log "Testing the package"
    pubkey=""
    if [ -n "${SIGN_KEY}" ]; then
        # Installed the way a user would: with the public key in /etc/signify.
        to_vm "${SIGN_KEY%.sec}.pub" default:/tmp/
        pubkey="/tmp/$(basename "${SIGN_KEY%.sec}.pub")"
    fi
    vm sudo ksh /tmp/test-package.sh "${pkg_path}" ${pubkey}
fi
