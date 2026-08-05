#!/bin/bash 
. ./generate-modules.sh
cabal install rpki-prover:exe:rpki-prover --overwrite-policy=always
