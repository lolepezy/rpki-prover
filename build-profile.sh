#!/bin/bash 
# cabal install builds a copy of the sdist outside the repository, see build-info/README.txt
. ./write-build-info.sh
trap 'rm -f build-info/git.txt' EXIT
cabal install rpki-prover:exe:profiler --overwrite-policy=always
