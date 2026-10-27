#!/bin/bash 
docker build - < Dockerfile.static-builder --tag rpki-prover-builder

# cabal install builds a copy of the sdist outside the repository, see
# build-info/README.txt
. ./write-build-info.sh
trap 'rm -f build-info/git.txt' EXIT

# NOTE: Dockerfile.static-builder must have cabal-install available
docker run --rm \
    -v "$(pwd)":/project:Z \
    -w /project \
    rpki-prover-builder \
    sh -c "cabal update && cabal install exe:rpki-prover \
        --enable-executable-static \
        --installdir=/project \
        --install-method=copy \
        --overwrite-policy=always"