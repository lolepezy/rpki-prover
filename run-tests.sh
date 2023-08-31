#!/bin/sh
cp package-template.yaml package.yaml
if [[ $(uname -m) == 'arm64' ]]; then
    EXTRA_LIBRARIES="--extra-include-dirs=/opt/homebrew/include --extra-lib-dirs=/opt/homebrew/lib"
    stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack-9.6.yaml ${EXTRA_LIBRARIES}
else 
    stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack-9.6.yaml && \
    stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack-8.10.yaml
fi
