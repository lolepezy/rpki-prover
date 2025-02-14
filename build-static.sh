#!/bin/bash 
docker build - < Dockerfile.static-builder --tag rpki-prover-builder

. ./src-hash.sh

# This is extremely ugly but that's the only working option so far
# to avoid attempts to build static binaries every time
cat package-template.yaml | sed 's/#1//g' > package.yaml 

stack install --docker --docker-image "rpki-prover-builder" \
    --no-nix rpki-prover:rpki-prover-static --stack-yaml=stack.yaml