#!/usr/bin/env bash
#
# Runs pkg/test-scripts/test-rpki-prover.sh inside a systemd-enabled Debian or
# Ubuntu container. Much faster than the Vagrant VMs, and enough to catch packaging
# mistakes; pkg/vagrant is still what should be run before tagging a release.
#
#   ./pkg/test-scripts/run-in-docker.sh bookworm
#   ./pkg/test-scripts/run-in-docker.sh --keep trixie
#   ./pkg/test-scripts/run-in-docker.sh --sandbox bookworm
#
# With --sandbox it runs test-worker-sandbox.sh instead: the daemon is left
# running long enough to spawn worker processes, and the journal is checked for
# workers that failed or that could not sandbox themselves. That one needs
# outbound network access and takes several minutes.
#
# Requires a container runtime that can run systemd (--privileged). Note that
# --privileged also means the container runs without Docker's seccomp profile,
# which would otherwise deny the landlock_* syscalls and make the sandbox look
# unavailable for reasons that have nothing to do with the package.

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
KEEP=0
SANDBOX=0
SERVICE_TIMEOUT=${SERVICE_TIMEOUT:-180}
WORKER_TIMEOUT=${WORKER_TIMEOUT:-600}
RSYNC_TIMEOUT=${RSYNC_TIMEOUT:-900}

usage() {
    echo "Usage: run-in-docker.sh [--keep] [--sandbox] [--timeout N] <codename>" >&2
}

CODENAME=""
while [ $# -gt 0 ]; do
    case "$1" in
        --keep)    KEEP=1; shift ;;
        --sandbox) SANDBOX=1; shift ;;
        --timeout) SERVICE_TIMEOUT=$2; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        -*)        usage; exit 1 ;;
        *)         CODENAME=$1; shift ;;
    esac
done
[ -n "${CODENAME}" ] || { usage; exit 1; }

PKG_OUT="${REPO_ROOT}/pkg/packages/${CODENAME}"
[ -d "${PKG_OUT}" ] || {
    echo "No packages for ${CODENAME}; run pkg/build-packages.sh ${CODENAME} first." >&2
    exit 1
}

base_image=$(awk -v c="${CODENAME}" '$1 == c { print $2 }' "${REPO_ROOT}/pkg/releases.conf")
[ -n "${base_image}" ] || { echo "${CODENAME} is not listed in pkg/releases.conf" >&2; exit 1; }

image="rpki-prover-systemd-test:${CODENAME}"
container="rpki-prover-test-${CODENAME}"

docker build \
    --file "${REPO_ROOT}/pkg/test-scripts/Dockerfile.systemd" \
    --build-arg "BASE_IMAGE=${base_image}" \
    --tag "${image}" \
    "${REPO_ROOT}/pkg/test-scripts"

docker rm -f "${container}" >/dev/null 2>&1 || true

cleanup() {
    if [ "${KEEP}" -eq 1 ]; then
        echo "Leaving container ${container} running (--keep); remove it with:"
        echo "  docker rm -f ${container}"
    else
        docker rm -f "${container}" >/dev/null 2>&1 || true
    fi
}
trap cleanup EXIT

docker run -d \
    --name "${container}" \
    --privileged \
    --cgroupns=host \
    --tmpfs /run --tmpfs /run/lock \
    --volume /sys/fs/cgroup:/sys/fs/cgroup:rw \
    --volume "${REPO_ROOT}/pkg:/opt/rpki-prover-pkg:ro" \
    "${image}" >/dev/null

echo "Waiting for systemd in ${container}..."
for _ in $(seq 60); do
    state=$(docker exec "${container}" systemctl is-system-running 2>/dev/null || true)
    case "${state}" in
        running|degraded) break ;;
    esac
    sleep 1
done
echo "systemd state: ${state:-unknown}"

docker exec \
    --env "SERVICE_TIMEOUT=${SERVICE_TIMEOUT}" \
    --env "WORKER_TIMEOUT=${WORKER_TIMEOUT}" \
    --env "SANDBOX=${SANDBOX}" \
    --env "RSYNC_TIMEOUT=${RSYNC_TIMEOUT}" \
    "${container}" \
    bash -c '
        set -e
        PKG_DIR="/opt/rpki-prover-pkg/packages/'"${CODENAME}"'"
        DEB=$(find "${PKG_DIR}" -maxdepth 1 -name "rpki-prover_*.deb" | sort | tail -1)
        apt-get update >/dev/null
        if [ "${SANDBOX}" = "1" ]; then
            exec /opt/rpki-prover-pkg/test-scripts/test-worker-sandbox.sh \
                --deb "${DEB}" --worker-timeout "${WORKER_TIMEOUT}" \
                --rsync-timeout "${RSYNC_TIMEOUT}"
        fi
        OLD_DEB=$(find "${PKG_DIR}/old" -maxdepth 1 -name "rpki-prover_*.deb" 2>/dev/null | sort | tail -1)
        ARGS=(--deb "${DEB}" --service-timeout "${SERVICE_TIMEOUT}")
        [ -n "${OLD_DEB}" ] && ARGS+=(--old-deb "${OLD_DEB}")
        exec /opt/rpki-prover-pkg/test-scripts/test-rpki-prover.sh "${ARGS[@]}" all
    '
