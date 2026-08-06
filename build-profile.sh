#!/bin/bash 
. ./generate-modules.sh
cabal install rpki-prover:exe:profiler --overwrite-policy=always
