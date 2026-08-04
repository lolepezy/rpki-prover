#!/bin/bash 
. ./src-hash.sh
cp package-template.yaml package.yaml
hpack
if [[ $(uname -m) == 'arm64' ]]; then
    EXTRA_FLAGS="--extra-include-dirs=/opt/homebrew/include --extra-lib-dirs=/opt/homebrew/lib"
fi
cabal install rpki-prover:exe:rpki-prover --overwrite-policy=always ${EXTRA_FLAGS}
