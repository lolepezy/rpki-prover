#!/bin/bash 
# cabal install builds a copy of the sdist outside the repository, see build-info/README.txt
. ./write-build-info.sh
trap 'rm -f build-info/git.txt' EXIT
cabal -j8 install rpki-prover:exe:rpki-prover --overwrite-policy=always
