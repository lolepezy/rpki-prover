docker build - < Dockerfile.static-builder --tag rpki-prover-builder && \
stack install --ghc-options \
  '-static -fPIC -threaded -O2 -Wall -rtsopts -funbox-strict-fields "-with-rtsopts=-A16m -AL64m -N2 -qg -T --disable-delayed-os-memory-return"' \
  rpki-prover:exe:rpki-prover \
  --docker --docker-image "rpki-prover-builder" \
  --stack-yaml=stack-8.10.yaml