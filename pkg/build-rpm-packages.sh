#!/usr/bin/env bash
#
# Builds rpki-prover RPM packages, one per target release, each inside a
# container based on that release. The RPM counterpart of
# pkg/build-packages.sh.
#
#   ./pkg/build-rpm-packages.sh                 # every release in pkg/rpm-releases.conf
#   ./pkg/build-rpm-packages.sh el9             # only this one
#   ./pkg/build-rpm-packages.sh --revision 2 el10
#
# Results end up in pkg/packages/<release>/.

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "${REPO_ROOT}"

RELEASES_CONF="pkg/rpm-releases.conf"
OUTPUT_DIR="${REPO_ROOT}/pkg/packages"
RPM_REVISION=1
MAINTAINER="Mikhail Puzanov <misha.puzanov@pm.me>"
DOCKER_BUILD_ARGS=()
REQUESTED=()

usage() {
    cat >&2 <<'USAGE'
Usage: pkg/build-rpm-packages.sh [options] [release...]

Options:
  -o, --output DIR    where to put the built packages (default: pkg/packages)
  -r, --revision N    the Release: number before %{dist} (default: 1)
  -m, --maintainer S  changelog maintainer
      --no-cache      rebuild the builder images from scratch
      --pull          always pull a newer base image
  -h, --help          this text

With no release arguments every release listed in pkg/rpm-releases.conf is built.
USAGE
}

while [ $# -gt 0 ]; do
    case "$1" in
        -o|--output)     OUTPUT_DIR=$(mkdir -p "$2" && cd "$2" && pwd); shift 2 ;;
        -r|--revision)   RPM_REVISION=$2; shift 2 ;;
        -m|--maintainer) MAINTAINER=$2; shift 2 ;;
        --no-cache)      DOCKER_BUILD_ARGS+=(--no-cache); shift ;;
        --pull)          DOCKER_BUILD_ARGS+=(--pull); shift ;;
        -h|--help)       usage; exit 0 ;;
        -*)              echo "Unknown option: $1" >&2; usage; exit 1 ;;
        *)               REQUESTED+=("$1"); shift ;;
    esac
done

command -v docker >/dev/null || { echo "docker is required" >&2; exit 1; }
[ -f "${RELEASES_CONF}" ] || { echo "${RELEASES_CONF} not found" >&2; exit 1; }

wanted() {
    [ ${#REQUESTED[@]} -eq 0 ] && return 0
    local r
    for r in "${REQUESTED[@]}"; do [ "$r" = "$1" ] && return 0; done
    return 1
}

# Read the whole config up front; see pkg/build-packages.sh for why.
entries=()
while read -r release image; do
    case "${release}" in ''|\#*) continue ;; esac
    entries+=("${release} ${image}")
done < "${RELEASES_CONF}"

built=()
matched=0

for entry in "${entries[@]}"; do
    read -r release image <<< "${entry}"
    wanted "${release}" || continue
    matched=$((matched + 1))

    image_tag="rpki-prover-rpm-builder:${release}"
    out="${OUTPUT_DIR}/${release}"
    mkdir -p "${out}"

    echo
    echo "############################################################"
    echo "# ${release} (${image}) -> ${out}"
    echo "############################################################"

    docker build \
        "${DOCKER_BUILD_ARGS[@]}" \
        --file pkg/Dockerfile.rpm-builder \
        --build-arg "BASE_IMAGE=${image}" \
        --tag "${image_tag}" \
        .

    docker run --rm \
        --volume "${REPO_ROOT}:/src:ro" \
        --volume "${out}:/out" \
        --env "RELEASE=${release}" \
        --env "RPM_REVISION=${RPM_REVISION}" \
        --env "MAINTAINER=${MAINTAINER}" \
        --env "HOST_UID=$(id -u)" \
        --env "HOST_GID=$(id -g)" \
        "${image_tag}" \
        /src/pkg/build-rpm-in-container.sh

    built+=("${release}")
done

if [ "${matched}" -eq 0 ]; then
    echo "None of the requested releases are listed in ${RELEASES_CONF}" >&2
    exit 1
fi

echo
echo "Built packages:"
for release in "${built[@]}"; do
    find "${OUTPUT_DIR}/${release}" -maxdepth 1 -name '*.rpm' -printf '  %p\n'
done
