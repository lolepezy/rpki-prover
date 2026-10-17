#!/usr/bin/env bash
#
# Builds every release artifact of a tagged version: the static binary, the
# docker image, the Debian/Ubuntu and RPM packages and the OpenBSD package.
# The docker image is pushed last, once everything else has been built.
#
#   ./release.sh v0.11.0                  # build everything, push the image
#   ./release.sh --no-push v0.11.0        # build everything, push nothing
#   ./release.sh --skip openbsd v0.11.0   # everything but the OpenBSD package
#   ./release.sh --only deb,rpm v0.11.0   # just these
#
# Without a tag argument, the tag pointing at HEAD is used.
#
# Everything is built from a fresh clone of the repository checked out at the
# tag, never from this working tree: uncommitted changes don't leak into a
# release, and the binaries don't claim "uncommitted files present".
#
# Artifacts end up in releases/<version>/:
#   rpki-prover-<version>-linux-amd64     static binary
#   deb/<codename>/                       Debian/Ubuntu packages
#   rpm/<release>/                        RPM packages
#   openbsd/<release>/                    OpenBSD package
#   SHA256SUMS

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "${REPO_ROOT}"

DOCKER_REPO=${DOCKER_REPO:-lolepezy/rpki-prover}
ALL_COMPONENTS=(static docker deb rpm openbsd)

PUSH=yes
KEEP_WORK=no
OPENBSD_ARGS=()
COMPONENTS=("${ALL_COMPONENTS[@]}")
TAG=""

usage() {
    cat >&2 <<'USAGE'
Usage: ./release.sh [options] [TAG]

Options:
      --only LIST            comma-separated components to build
                             (static, docker, deb, rpm, openbsd; default: all)
      --skip LIST            comma-separated components not to build
      --no-push              don't push the docker image
      --openbsd-release R    OpenBSD release to build for (default: that of
                             pkg/openbsd/build-package.sh)
      --openbsd-sign KEY     sign the OpenBSD package with this signify key
      --keep-work            keep the clone the release was built from
  -h, --help                 this text
USAGE
}

split_list() { tr ',' ' ' <<< "$1"; }

while [ $# -gt 0 ]; do
    case "$1" in
        --only)            read -ra COMPONENTS <<< "$(split_list "$2")"; shift 2 ;;
        --skip)
            skipped=" $(split_list "$2") "
            kept=()
            for c in "${COMPONENTS[@]}"; do
                [[ "${skipped}" == *" ${c} "* ]] || kept+=("${c}")
            done
            COMPONENTS=("${kept[@]}")
            shift 2 ;;
        --no-push)         PUSH=no; shift ;;
        --openbsd-release) OPENBSD_ARGS+=(--release "$2"); shift 2 ;;
        --openbsd-sign)    OPENBSD_ARGS+=(--sign "$(realpath "$2")"); shift 2 ;;
        --keep-work)       KEEP_WORK=yes; shift ;;
        -h|--help)         usage; exit 0 ;;
        -*)                echo "Unknown option: $1" >&2; usage; exit 1 ;;
        *)                 TAG=$1; shift ;;
    esac
done

log()  { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
fail() { echo "$*" >&2; exit 1; }

want() {
    local c
    for c in "${COMPONENTS[@]}"; do [ "${c}" = "$1" ] && return 0; done
    return 1
}

for c in "${COMPONENTS[@]}"; do
    [[ " ${ALL_COMPONENTS[*]} " == *" ${c} "* ]] || fail "Unknown component: ${c}"
done
[ ${#COMPONENTS[@]} -gt 0 ] || fail "Nothing to build"

# ---------------------------------------------------------------------------
# Check the tag
# ---------------------------------------------------------------------------
if [ -z "${TAG}" ]; then
    TAG=$(git describe --exact-match --tags HEAD 2>/dev/null) ||
        fail "HEAD has no tag; tag it or pass the tag explicitly"
fi

TAG_COMMIT=$(git rev-parse --verify --quiet "refs/tags/${TAG}^{commit}") ||
    fail "No such tag: ${TAG}"

VERSION=$(git show "${TAG_COMMIT}:rpki-prover.cabal" | awk '/^version:/ { print $2; exit }')
[ "${TAG}" = "v${VERSION}" ] ||
    fail "Tag ${TAG} doesn't match the version in rpki-prover.cabal at that tag (${VERSION})"

# A pushed image refers to a tag that everyone should be able to check out.
REMOTE_COMMIT=$(git ls-remote origin "refs/tags/${TAG}^{}" "refs/tags/${TAG}" |
                awk 'NR == 1 || /\^\{\}$/ { c = $1 } END { print c }')
if [ "${REMOTE_COMMIT}" != "${TAG_COMMIT}" ]; then
    msg="Tag ${TAG} is not on origin, or points at a different commit there"
    if want docker && [ "${PUSH}" = yes ]; then
        fail "${msg}; push it first (or use --no-push)"
    fi
    echo "Warning: ${msg}" >&2
fi

command -v docker >/dev/null || fail "docker is required"
if want openbsd; then
    command -v vagrant >/dev/null || fail "vagrant is required for the OpenBSD package"
fi

OUT_DIR="${REPO_ROOT}/releases/${VERSION}"
WORK="${REPO_ROOT}/releases/.work-${VERSION}"
SRC="${WORK}/rpki-prover"

cleanup() {
    if [ "${KEEP_WORK}" = no ]; then
        rm -rf "${WORK}"
    else
        echo "The clone is kept in ${SRC}" >&2
    fi
}
trap cleanup EXIT

# ---------------------------------------------------------------------------
# A clean clone at the tag
# ---------------------------------------------------------------------------
log "Cloning ${TAG} ($(git rev-parse --short "${TAG_COMMIT}")) into ${SRC}"
rm -rf "${WORK}"
mkdir -p "${WORK}" "${OUT_DIR}"
# A full clone rather than a shallow one or a worktree: generate-modules.sh
# counts the commits, and the package builds copy .git into containers, where
# a worktree's pointer to this repository's .git would dangle.
git clone --quiet --no-checkout "${REPO_ROOT}" "${SRC}"

# generate-modules.sh records the branch, so build on the branch the tag was
# made on rather than on a detached HEAD.
BRANCH=$(git rev-parse --abbrev-ref HEAD)
if git merge-base --is-ancestor "${TAG_COMMIT}" HEAD 2>/dev/null && [ "${BRANCH}" != HEAD ]; then
    git -C "${SRC}" checkout --quiet -B "${BRANCH}" "${TAG_COMMIT}"
else
    git -C "${SRC}" checkout --quiet --detach "${TAG_COMMIT}"
fi

# Each build runs generate-modules.sh, which rewrites two tracked files. Undo
# that before the next build, so that it doesn't see its tree as dirty.
pristine() {
    git -C "${SRC}" checkout --quiet -- .
    [ -z "$(git -C "${SRC}" status --porcelain)" ] ||
        fail "The clone in ${SRC} has unexpected changes: $(git -C "${SRC}" status --porcelain)"
}

check_version() {
    local got
    got=$("$@" --version)
    [ "${got}" = "rpki-prover-${VERSION}" ] ||
        fail "Expected rpki-prover-${VERSION} from '$*', got '${got}'"
}

built=()

# ---------------------------------------------------------------------------
# Static binary
# ---------------------------------------------------------------------------
if want static; then
    log "Building the static binary"
    pristine
    docker build - < "${SRC}/Dockerfile.static-builder" --tag rpki-prover-builder
    (cd "${SRC}" && bash ./generate-modules.sh)
    # The container runs as root; hand what it writes back to us, or the
    # clone can't be removed.
    docker run --rm \
        --volume "${SRC}:/project:Z" \
        --workdir /project \
        rpki-prover-builder \
        sh -c "cabal update && cabal install exe:rpki-prover \
                    --enable-executable-static \
                    --installdir=/project \
                    --install-method=copy \
                    --overwrite-policy=always; \
               status=\$?; chown -R $(id -u):$(id -g) /project; exit \$status"
    static_binary="${OUT_DIR}/rpki-prover-${VERSION}-linux-amd64"
    mv "${SRC}/rpki-prover" "${static_binary}"
    check_version "${static_binary}"
    built+=("${static_binary}")
fi

# ---------------------------------------------------------------------------
# Docker image
# ---------------------------------------------------------------------------
if want docker; then
    log "Building the docker image ${DOCKER_REPO}:${VERSION}"
    pristine
    (cd "${SRC}" && bash ./generate-modules.sh)
    docker build "${SRC}" --file "${SRC}/Dockerfile.prover" --tag "${DOCKER_REPO}:${VERSION}"
    check_version docker run --rm "${DOCKER_REPO}:${VERSION}"
    docker tag "${DOCKER_REPO}:${VERSION}" "${DOCKER_REPO}:latest"
    built+=("docker image ${DOCKER_REPO}:${VERSION} (also tagged latest)")
fi

# ---------------------------------------------------------------------------
# Debian/Ubuntu and RPM packages
# ---------------------------------------------------------------------------
if want deb; then
    log "Building the Debian/Ubuntu packages"
    pristine
    "${SRC}/pkg/build-packages.sh" --output "${OUT_DIR}/deb"
    built+=("${OUT_DIR}/deb")
fi

if want rpm; then
    log "Building the RPM packages"
    pristine
    "${SRC}/pkg/build-rpm-packages.sh" --output "${OUT_DIR}/rpm"
    built+=("${OUT_DIR}/rpm")
fi

# ---------------------------------------------------------------------------
# OpenBSD package
# ---------------------------------------------------------------------------
if want openbsd; then
    log "Building the OpenBSD package"
    pristine
    # Use the VM of this checkout rather than creating one for the clone:
    # setting one up takes a long time and a lot of disk.
    VAGRANT_CWD="${REPO_ROOT}/pkg/openbsd" \
        "${SRC}/pkg/openbsd/build-package.sh" "${OPENBSD_ARGS[@]}"
    mkdir -p "${OUT_DIR}/openbsd"
    cp -r "${SRC}/pkg/openbsd/packages/." "${OUT_DIR}/openbsd/"
    built+=("${OUT_DIR}/openbsd")
fi

# ---------------------------------------------------------------------------
# Checksums, and only then publish
# ---------------------------------------------------------------------------
log "Writing SHA256SUMS"
(cd "${OUT_DIR}" &&
    find . -type f \( -name 'rpki-prover-*' -o -name '*.deb' -o -name '*.rpm' -o -name '*.tgz' \) \
        -printf '%P\n' | sort | xargs -r sha256sum > SHA256SUMS)

if want docker; then
    if [ "${PUSH}" = yes ]; then
        log "Pushing ${DOCKER_REPO}:${VERSION} and ${DOCKER_REPO}:latest"
        docker push "${DOCKER_REPO}:${VERSION}"
        docker push "${DOCKER_REPO}:latest"
    else
        echo "Not pushing; to publish the image later:" >&2
        echo "  docker push ${DOCKER_REPO}:${VERSION} && docker push ${DOCKER_REPO}:latest" >&2
    fi
fi

log "Release ${VERSION} built"
for b in "${built[@]}"; do echo "  ${b}"; done
echo "  ${OUT_DIR}/SHA256SUMS"
