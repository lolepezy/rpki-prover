
## Setting up development environment

- You need `stack` to start (https://docs.haskellstack.org/en/stable/README/)
- Docker
- Some libraries
    * On Linux using apt-get, that will be : `sudo apt install rsync libz-dev libexpat1-dev liblmdb-dev liblzma-dev libgmp-dev`.
    * On MacOS using brew, that will be: `brew install rsync lmdb xz expat`.

## Build

- There's an annoying bug is the stack where `stack build rpki-prover:rpki-prover` always tries to build `rpki-prover-static` executable as well and, obviously, fails. This problem exists since forever
https://github.com/commercialhaskell/stack/issues/1406 and nobody gives a crap. That's why there is `package-template.yaml` that is being transformed to `package.yaml` depending on what kind of binary we want to build, normal or static. This can be fixed by moving to Cabal somewhere in the future.

So
- Full build is done by the `./build-local.sh` script, it should normally take 30-40 minutes on an average computer.
- Tests can be run using `./run-tests.sh`.
- The command for using `ghcid` is 

    ```ghcid --command="stack repl rpki-prover:lib rpki-prover:rpki-prover"``` 
or 

     ```ghcid --command="stack repl rpki-prover:lib rpki-prover:test:rpki-prover-test"```  for the set of modules including tests.

- For local docker image build use 

     ```cp package-template.yaml package.yaml && docker build . --file Dockerfile.prover --tag lolepezy/rpki-prover:latest```

## Bumping DB version

There is a value `currentDatabaseVersion` definted in `Database.hs`, that needs to be increased every time serialisation/deserialisation of the cache may break. In practice, that means whenever there's any change to the data types in `Domain.hs`. Changes in involved library types will also break serialisation, so essentially it's better to bump `currentDatabaseVersion` with any change of stack snapshot version of library version. Version change will result in prover erasing its cache and starting from scratch after upgrade and restart, which is a minor nuisance compared to processing serialisation errors.

## Releasing

At the moment releasing is done manually, since github actions consistently fail with out-of-memory errors.

For a release version `X.Y.Z` the procedure is this:

- Update version in the `package-template.yaml` file (TODO Make it automated?)
- `git tag -a vX.Y.Z -m "Release X.Y.Z"` 
- `git push -f --tags`
- Create and push docker image with `./docker-release.sh X.Y.Z`
- Build static binary with `./build-static.sh`
- Create a release in github UI using `vX.Y.Z` tag and upload the the static binary to the artifacts of the release.

For now, the latest LTS for which static binary builds without issues is lts-22.20. And Dockerfile.static-builder should contain 
`FROM utdemir/ghc-musl:v25-ghc964 as rpki-prover-builder` for the pipeling to work. That will be fixed somewhere in the future after moving to later versions of GHC.
