#!/bin/bash 
export RELEASE=$1

. ./generate-modules.sh

docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:${RELEASE} && \
docker tag lolepezy/rpki-prover:${RELEASE} lolepezy/rpki-prover:latest && \
docker push lolepezy/rpki-prover:${RELEASE} && \
docker push lolepezy/rpki-prover:latest
