#!/bin/bash 
. ./src-hash.sh
cp package-template.yaml package.yaml
if [[ $(uname -m) == 'arm64' ]]; then
    EXTRA_LIBRARIES="--extra-include-dirs=/opt/homebrew/include --extra-lib-dirs=/opt/homebrew/lib"
fi
stack install rpki-prover:exe:rpki-prover ${EXTRA_LIBRARIES}
