
## Setting up development environment

- You need `stack` to start (https://docs.haskellstack.org/en/stable/README/)
- Full build is done by `stack install rpki-prover:rpki-prover`, it should normally take 30-40 minutes on an average computer.
- Tests can be run using `stack test rpki-prover:lib rpki-prover:test:rpki-prover-test`.
- The command for using `ghcid` is `ghcid --command="stack repl rpki-prover:lib rpki-prover:rpki-prover"` 
  or `ghcid --command="stack repl rpki-prover:lib rpki-prover:test:rpki-prover-test" ` for the set of modules includsing tests.
 
From time to time during `stack install ...` the following (up to the line numbers) can happen
```
rpki-prover> build (lib + exe)
Preprocessing library for rpki-prover-0.1.0.0..
Building library for rpki-prover-0.1.0.0..
[43 of 70] Compiling RPKI.Orphans.Json [RPKI.Config changed]

.../rpki-prover/src/RPKI/Orphans/Json.hs:246:10: error:
    • No instance for (GToJSON'
                         Value Zero (GHC.Generics.Rep Crypto.PubKey.RSA.Types.PublicKey))
        arising from a use of ‘aeson-1.5.6.0:Data.Aeson.Types.ToJSON.$dmtoJSON’
    • In the expression:
        aeson-1.5.6.0:Data.Aeson.Types.ToJSON.$dmtoJSON
          @(Crypto.PubKey.RSA.Types.PublicKey)
      In an equation for ‘toJSON’:
          toJSON
            = aeson-1.5.6.0:Data.Aeson.Types.ToJSON.$dmtoJSON
                @(Crypto.PubKey.RSA.Types.PublicKey)
      In the instance declaration for
        ‘ToJSON Crypto.PubKey.RSA.Types.PublicKey’
    |
246 | instance ToJSON Crypto.PubKey.RSA.Types.PublicKey
```

It is related to the way orphan instances are derived for some library types. Something is probably wrong 
in the way modules are organised, but it looks more like a compiler quirk. In some cases it helps to just rerun the `stack install (or test)...` command, 
otherwise `stack clean` and full rebuild helps. It's annoying but I haven't found a way to fix it.


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

Update version in the package.yaml file (TODO Make it automated?)
```
git tag -a vX.Y.Z -m "Release X.Y.Z"
git push -f --tags
```
Github action will kick in and build the static binary and create Dockerhub image. Action usually creates ugly and fucked up releases, so manual involvment is necessary afterwards.
