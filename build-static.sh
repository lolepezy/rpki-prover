#!/bin/bash 
docker build - < Dockerfile.static-builder --tag rpki-prover-builder

. ./src-hash.sh

# Uncomment static-build flags (lines marked #1) in package.yaml, then regenerate .cabal
cat package-template.yaml | sed 's/#1//g' > package.yaml
hpack

# NOTE: Dockerfile.static-builder must have cabal-install available
docker run --rm \
    -v "$(pwd)":/project:Z \
    -w /project \
    rpki-prover-builder \
    cabal install rpki-prover:exe:rpki-prover --overwrite-policy=always