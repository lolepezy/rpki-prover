#!/bin/sh
. ./generate-modules.sh
cp package-template.yaml package.yaml
hpack
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest