#!/bin/sh
. ./src-hash.sh
cp package-template.yaml package.yaml
hpack
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest