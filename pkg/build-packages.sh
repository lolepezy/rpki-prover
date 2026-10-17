#!/usr/bin/env bash
#
# Builds rpki-prover Debian/Ubuntu packages, one per target release, each inside a
# container based on that release.
#
#   ./pkg/build-packages.sh                 # every release in pkg/releases.conf
#   ./pkg/build-packages.sh bookworm trixie # only these
#   ./pkg/build-packages.sh --output /tmp/debs sid
#
# Results end up in pkg/packages/<codename>/.

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "${REPO_ROOT}"

RELEASES_CONF="pkg/releases.conf"
OUTPUT_DIR="${REPO_ROOT}/pkg/packages"
DEB_REVISION=1
MAINTAINER="Mikhail Puzanov <misha.puzanov@pm.me>"
DOCKER_BUILD_ARGS=()
REQUESTED=()

usage() {
    cat >&2 <<'USAGE'
Usage: pkg/build-packages.sh [options] [codename...]

Options:
  -o, --output DIR    where to put the built packages (default: pkg/packages)
  -r, --revision N    debian revision to use (default: 1)
  -m, --maintainer S  changelog maintainer
      --no-cache      rebuild the builder images from scratch
      --pull          always pull a newer base image
  -h, --help          this text

With no codename arguments every release listed in pkg/releases.conf is built.
USAGE
}

while [ $# -gt 0 ]; do
    case "$1" in
        -o|--output)     OUTPUT_DIR=$(mkdir -p "$2" && cd "$2" && pwd); shift 2 ;;
        -r|--revision)   DEB_REVISION=$2; shift 2 ;;
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

# Read the whole config up front rather than looping over a redirected file:
# docker can consume the loop's stdin, which would silently swallow every
# release after the first.
entries=()
while read -r codename image suffix distribution; do
    case "${codename}" in ''|\#*) continue ;; esac
    entries+=("${codename} ${image} ${suffix} ${distribution}")
done < "${RELEASES_CONF}"

built=()
matched=0

for entry in "${entries[@]}"; do
    read -r codename image suffix distribution <<< "${entry}"
    wanted "${codename}" || continue
    matched=$((matched + 1))

    # "-" means "no suffix" in releases.conf, since the column cannot be empty.
    [ "${suffix}" = "-" ] && suffix=""

    image_tag="rpki-prover-deb-builder:${codename}"
    out="${OUTPUT_DIR}/${codename}"
    mkdir -p "${out}"

    echo
    echo "############################################################"
    echo "# ${codename} (${image}) -> ${out}"
    echo "############################################################"

    docker build \
        "${DOCKER_BUILD_ARGS[@]}" \
        --file pkg/Dockerfile.deb-builder \
        --build-arg "BASE_IMAGE=${image}" \
        --tag "${image_tag}" \
        .

    docker run --rm \
        --volume "${REPO_ROOT}:/src:ro" \
        --volume "${out}:/out" \
        --env "CODENAME=${codename}" \
        --env "DISTRIBUTION=${distribution}" \
        --env "VERSION_SUFFIX=${suffix}" \
        --env "DEB_REVISION=${DEB_REVISION}" \
        --env "MAINTAINER=${MAINTAINER}" \
        --env "HOST_UID=$(id -u)" \
        --env "HOST_GID=$(id -g)" \
        "${image_tag}" \
        /src/pkg/build-deb-in-container.sh

    built+=("${codename}")
done

if [ "${matched}" -eq 0 ]; then
    echo "None of the requested releases are listed in ${RELEASES_CONF}" >&2
    exit 1
fi

echo
echo "Built packages:"
for codename in "${built[@]}"; do
    find "${OUTPUT_DIR}/${codename}" -name '*.deb' -printf '  %p\n'
done
