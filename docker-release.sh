export RELEASE=$1

. ./src-hash.sh
cp package-template.yaml package.yaml

docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:${RELEASE} && \
docker tag lolepezy/rpki-prover:${RELEASE} lolepezy/rpki-prover:latest && \
docker push lolepezy/rpki-prover:${RELEASE} && \
docker push lolepezy/rpki-prover:latest
