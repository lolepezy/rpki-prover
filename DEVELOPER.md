
## Setting up development environment

- You need `stack` to start (https://docs.haskellstack.org/en/stable/README/)
- Full build is done by `stack install rpki-prover:rpki-prover`, it should normally take 30-40 minutes on an average computer.
- Tests can be run using `stack test rpki-prover:lib rpki-prover:test:rpki-prover-test`.
- The command for using `ghcid` is `ghcid --command="stack repl rpki-prover:lib rpki-prover:rpki-prover"` 
  or `ghcid --command="stack repl rpki-prover:lib rpki-prover:test:rpki-prover-test" ` for the set of modules including tests.
 

## Bumping DB version

There is a value `currentDatabaseVersion` definted in `Database.hs`, that needs to be increased every time serialisation/deserialisation of the cache may break. In practice, that means whenever there's any change to the data types in `Domain.hs`. Changes in involved library types will also break serialisation, so essentially it's better to bump `currentDatabaseVersion` with any change of stack snapshot version of library version. Version change will result in prover erasing its cache and starting from scratch after upgrade and restart, which is a minor nuisance compared to processing serialisation errors.


## Docker build

```
export RELEASE=X.Y.Z 
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:${RELEASE} && \
docker tag lolepezy/rpki-prover:${RELEASE} lolepezy/rpki-prover:latest && \
docker push lolepezy/rpki-prover:${RELEASE} && \
docker push lolepezy/rpki-prover:latest
```

## Building static Linux executable

```
docker build - < Dockerfile.static-builder --tag rpki-prover-builder
stack install --docker --docker-image "rpki-prover-builder" --no-nix rpki-prover:rpki-prover-static
```

## Releasing

NOTE: At the moment releasing is done manually, since a lot there is broken (there are intricate dependencies between the OS versions, libraries and Stackage snapshot). Also Github actions started to crash with out-of-memory errors.

Update version in the package.yaml file (TODO Make it automated?)
```
git tag -a vX.Y.Z -m "Release X.Y.Z"
git push -f --tags
```
Github action will kick in and build the static binary and create Dockerhub image. Action usually creates ugly and fucked up releases/release drafts, so manual involvement is necessary afterwards, but it's minimal and rare.
