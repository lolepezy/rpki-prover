#!/bin/sh

cd ..

docker build - < Dockerfile.static-builder --tag rpki-prover-builder
stack install --docker --docker-image "rpki-prover-builder" --no-nix rpki-prover:rpki-prover-static