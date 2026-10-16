#!/bin/ksh
#
# Runs inside the OpenBSD VM as root: builds the package from the port in
# /build/upload/port, optionally from a source tarball of the working tree
# instead of the release tarball that distinfo was made for.
#
#   build-port.sh [LOCAL_DISTFILE]

set -eu

LOCAL_DISTFILE=${1:-}

# Without PORTS_PRIVSEP the build runs as root with the limits of whoever ran
# sudo, a 1.5G datasize for the "staff" class, and GHC runs out of it on
# rpki-prover's own modules ("Unable to commit 1048576 bytes of memory").
ulimit -d "$(ulimit -Hd)"
PORT=/usr/ports/mystuff/net/rpki-prover

rm -rf "${PORT}"
mkdir -p "$(dirname "${PORT}")"
cp -R /build/upload/port "${PORT}"

# The ports tree refuses users that are not in its registry. 906 is the next
# free uid as of 2026-10; a port in the official tree gets its own.
grep -q '^906 _rpki-prover' /usr/ports/infrastructure/db/user.list ||
    printf '906 _rpki-prover\t\t_rpki-prover\t\tnet/rpki-prover\n' \
        >> /usr/ports/infrastructure/db/user.list

cd "${PORT}"
make clean=all >/dev/null

# Some 300 files come from Hackage, and its CDN now and then breaks off a
# transfer ("ftp: Invalid chunk size"). Whatever did arrive is kept.
retry() {
    for attempt in 1 2 3; do
        "$@" && return 0
        echo "Attempt ${attempt} of '$*' failed" >&2
    done
    return 1
}

if [ -n "${LOCAL_DISTFILE}" ]; then
    distdir=$(make show=FULLDISTDIR)
    distname=$(make show=DISTNAME).tar.gz
    install -d -o _pfetch "${distdir}"
    install -o _pfetch -m 644 "${LOCAL_DISTFILE}" "${distdir}/${distname}"
    # Fetches the dependencies and records checksums for everything,
    # including the local tarball, in this copy of the port only.
    retry make makesum
elif [ ! -f distinfo ]; then
    # The first build of a new release tag: record what was fetched.
    retry make makesum
else
    retry make fetch
fi

make package
make port-lib-depends-check

ls "$(make show=PACKAGE_REPOSITORY)/$(machine -a)/all/$(make show=FULLPKGNAME).tgz"
