#!/bin/bash 
. ./generate-modules.sh
cabal -j8 install rpki-prover:exe:rpki-prover --overwrite-policy=always
