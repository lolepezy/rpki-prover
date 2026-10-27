#!/bin/bash
# The docker build context has no .git, see build-info/README.txt
. ./write-build-info.sh
trap 'rm -f build-info/git.txt' EXIT
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest
