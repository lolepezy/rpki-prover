
## Setting up development environment

- You need `stack` to start (https://docs.haskellstack.org/en/stable/README/)
- Full build is done by `stack install rpki-prover:rpki-prover`, it should normally take 30-40 minutes on an average computer.
- Tests can be run using `stack test rpki-prover:lib rpki-prover:test:rpki-prover-test`.
- The command for using `ghcid` is `ghcid --command="stack repl rpki-prover:lib rpki-prover:rpki-prover"` 
  or `ghcid --command="stack repl rpki-prover:lib rpki-prover:test:rpki-prover-test" ` for the set of modules including tests.
 

## Bumping DB version

There is a value `currentDatabaseVersion` definted in `Database.hs`, that needs to be increased every time serialisation/deserialisation of the cache may break. In practice, that means whenever there's any change to the data types in `Domain.hs`. Changes in involved library types will also break serialisation, so essentially it's better to bump `currentDatabaseVersion` with any change of stack snapshot version of library version. Version change will result in prover erasing its cache and starting from scratch after upgrade and restart, which is a minor nuisance compared to processing serialisation errors.

## Build

1) Currently two different versions of the GHC are supported: 9.6 for the normal builds and docker images and 8.10.x for creating static binaries. There is a bug in the combination `ghc + alpine + musl + template haskell` leading to the compiler segfault for at least GHC 9.2, 9.4 and 9.6. That's why 8.10 is being dragged along.

2) It is important to run tests for both compiler versions, so `./run-tests.sh` is the way to go.

3) Another annoying bug is the stack bug where `stack build rpki-prover:rpki-prover` always tries to build `rpki-prover-static` executable as well and, obviously, fails. This problem exists since forever
https://github.com/commercialhaskell/stack/issues/1406 and nobody gives a crap. That's why there is `package-template.yaml` that is being transformed to `package.yaml` depending on what kind of binary we want to build, normal or static.

While (3) can be solved by moving from stack to cabal, there is no workaround for (1) so far (https://gitlab.haskell.org/ghc/ghc/-/issues/23043). As soon as the bug is fixed, 8.10 will be ditched.



## Docker build

To locally build the image use 
```
docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest
```

to build release image, use the script

```
./docker-release.sh X.Y.Z 
```
where `X.Y.Z` is the release version.


## Building static Linux executable

```
./build-static.sh
```

## Releasing

NOTE: At the moment releasing is done manually, since github actions consistently fail with out-of-memory errors.

The procedure is this:

- Update version in the package.yaml file (TODO Make it automated?)
- `git tag -a vX.Y.Z -m "Release X.Y.Z"`
- `git push -f --tags`
- `./docker-release.sh X.Y.Z`
- `./build-static.sh`
- Create release in github UI using `vX.Y.Z` tag and upload the the static binary to the artifacts.
