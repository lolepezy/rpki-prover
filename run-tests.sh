#!/bin/sh
cp package-template.yaml package.yaml
stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack.yaml