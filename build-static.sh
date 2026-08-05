#!/bin/bash 
docker build - < Dockerfile.static-builder --tag rpki-prover-builder

. ./generate-modules.sh

# NOTE: Dockerfile.static-builder must have cabal-install available
docker run --rm \
    -v "$(pwd)":/project:Z \
    -w /project \
    rpki-prover-builder \
    cabal install rpki-prover:exe:rpki-prover --overwrite-policy=always