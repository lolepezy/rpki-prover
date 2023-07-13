#!/bin/sh

stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack-9.6.yaml
stack test rpki-prover:lib rpki-prover:test:rpki-prover-test --stack-yaml=stack-8.10.yaml