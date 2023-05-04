
## Setting up development environment

- You need `stack` to start (https://docs.haskellstack.org/en/stable/README/)
- Full build is done by `stack install rpki-prover:rpki-prover`, it should normally take 30-40 minutes on an average computer.
- Tests can be run using `stack test rpki-prover:lib rpki-prover:test:rpki-prover-test`.
- The command for using `ghcid` is `ghcid --command="stack repl rpki-prover:lib rpki-prover:rpki-prover"` 
  or `ghcid --command="stack repl rpki-prover:lib rpki-prover:test:rpki-prover-test" ` for the set of modules including tests.
 

## Docker build

```
docker build . --file Dockerfile.prover --tag rpki-prover 
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
