#!/bin/sh

cp package-template.yaml package.yaml 
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest