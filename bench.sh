#!/bin/bash 
cp package-template.yaml package.yaml
if [[ $(uname -m) == 'arm64' ]]; then
    EXTRA_LIBRARIES="--extra-include-dirs=/opt/homebrew/include --extra-lib-dirs=/opt/homebrew/lib"
fi
stack clean
stack build --bench --no-run-benchmarks ${EXTRA_LIBRARIES}
stack bench :map-vs-hashmap-bench ${EXTRA_LIBRARIES}
