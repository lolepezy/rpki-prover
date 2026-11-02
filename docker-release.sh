#!/bin/bash 
export RELEASE=$1

# The docker build context has no .git, see build-info/README.txt
. ./write-build-info.sh
trap 'rm -f build-info/git.txt' EXIT

docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:${RELEASE} && \
docker tag lolepezy/rpki-prover:${RELEASE} lolepezy/rpki-prover:latest && \
docker push lolepezy/rpki-prover:${RELEASE} && \
docker push lolepezy/rpki-prover:latest
