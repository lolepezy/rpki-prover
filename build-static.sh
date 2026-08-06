#!/bin/bash 
docker build - < Dockerfile.static-builder --tag rpki-prover-builder

. ./generate-modules.sh

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