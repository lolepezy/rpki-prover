#!/bin/sh
. ./generate-modules.sh
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest