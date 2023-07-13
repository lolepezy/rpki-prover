stack install --ghc-options \
  '-threaded -O2 -Wall -rtsopts -funbox-strict-fields "-with-rtsopts=-A16m -AL64m -N2 -qg -T --disable-delayed-os-memory-return"' \
  rpki-prover:exe:rpki-prover --stack-yaml=stack-9.6.yaml