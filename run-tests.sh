#!/bin/sh
cp package-template.yaml package.yaml
if [[ $(uname -m) == 'arm64' ]]; then
    EXTRA_LIBRARIES="--extra-include-dirs=/opt/homebrew/include --extra-lib-dirs=/opt/homebrew/lib"
fi
stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack.yaml