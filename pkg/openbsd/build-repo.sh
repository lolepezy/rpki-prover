#!/usr/bin/env bash
#
# Turns pkg/openbsd/packages/<release>/*.tgz into a package directory that
# pkg_add can use over HTTPS, served by any static web server -- GitHub Pages
# in particular, next to the APT repository:
#
#   <url>/rpki-prover-pkg.pub
#   <url>/<release>/<arch>/rpki-prover-<version>.tgz
#   <url>/<release>/<arch>/index.html     the listing pkg_add reads
#
#   ./pkg/openbsd/build-repo.sh --pubkey rpki-prover-pkg.pub \
#       --url https://lolepezy.github.io/rpki-prover/openbsd
#
# The packages must already be signed (build-package.sh --sign) with the key
# that --pubkey belongs to; this script checks that they are.

set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

PACKAGES_DIR="${HERE}/packages"
OUTPUT_DIR="${HERE}/public"
PUBKEY=""
ARCH=amd64
BASE_URL="https://lolepezy.github.io/rpki-prover/openbsd"

usage() {
    cat >&2 <<'USAGE'
Usage: build-repo.sh --pubkey FILE [options]

Options:
  -k, --pubkey FILE   signify public key the packages are signed with; its
                      name must end in "pkg.pub" for pkg_add to accept it
  -p, --packages DIR  directory with <release>/*.tgz (default: pkg/openbsd/packages)
  -o, --output DIR    directory to generate (default: pkg/openbsd/public)
  -a, --arch ARCH     architecture the packages are for (default: amd64)
  -u, --url URL       public URL of the output directory, for the instructions
  -h, --help          this text
USAGE
}

while [ $# -gt 0 ]; do
    case "$1" in
        -k|--pubkey)   PUBKEY=$(readlink -f "$2"); shift 2 ;;
        -p|--packages) PACKAGES_DIR=$(readlink -f "$2"); shift 2 ;;
        -o|--output)   OUTPUT_DIR=$2; shift 2 ;;
        -a|--arch)     ARCH=$2; shift 2 ;;
        -u|--url)      BASE_URL=${2%/}; shift 2 ;;
        -h|--help)     usage; exit 0 ;;
        *)             echo "Unknown option: $1" >&2; usage; exit 1 ;;
    esac
done

[ -n "${PUBKEY}" ] || { usage; exit 1; }
KEY_NAME=$(basename "${PUBKEY}")
case "${KEY_NAME}" in
    *pkg.pub) ;;
    *) echo "pkg_add only trusts keys named *pkg.pub, not ${KEY_NAME}" >&2; exit 1 ;;
esac

rm -rf "${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}"
cp "${PUBKEY}" "${OUTPUT_DIR}/"

releases=()
for dir in "${PACKAGES_DIR}"/*/; do
    release=$(basename "${dir}")
    shopt -s nullglob
    pkgs=("${dir}"*.tgz)
    shopt -u nullglob
    [ ${#pkgs[@]} -gt 0 ] || continue

    out="${OUTPUT_DIR}/${release}/${ARCH}"
    mkdir -p "${out}"
    {
        echo "<!doctype html><title>rpki-prover for OpenBSD ${release}/${ARCH}</title>"
        echo "<ul>"
        for pkg in "${pkgs[@]}"; do
            name=$(basename "${pkg}")
            # pkg_sign puts the signify signature, which names the key, into
            # the header of the package's first gzip member.
            if ! head -c 1024 "${pkg}" | grep -aq "verify with ${KEY_NAME}\$"; then
                echo "${pkg} is not signed with ${KEY_NAME}" >&2
                exit 1
            fi
            cp "${pkg}" "${out}/"
            echo "<li><a href=\"${name}\">${name}</a></li>"
        done
        echo "</ul>"
    } > "${out}/index.html"
    releases+=("${release}")
done

[ ${#releases[@]} -gt 0 ] || { echo "No packages under ${PACKAGES_DIR}" >&2; exit 1; }

cat > "${OUTPUT_DIR}/index.html" <<EOF
<!doctype html>
<meta charset="utf-8">
<title>rpki-prover for OpenBSD</title>
<h1>rpki-prover for OpenBSD</h1>
<p>Packages for OpenBSD ${releases[*]} (${ARCH}). As root:</p>
<pre>
ftp -o /etc/signify/${KEY_NAME} ${BASE_URL}/${KEY_NAME}
export PKG_PATH=${BASE_URL}/%v/%a/:installpath
pkg_add rpki-prover
rcctl enable rpki_prover
rcctl start rpki_prover
</pre>
<p>Upgrades work the same way, with <code>pkg_add -u rpki-prover</code> and the
same PKG_PATH. Dependencies come from the mirror in /etc/installurl.
See /usr/local/share/doc/pkg-readmes/rpki-prover once it is installed.</p>
EOF

echo "Repository for ${releases[*]} in ${OUTPUT_DIR}"
