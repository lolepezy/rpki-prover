
## Setting up development environment

- You need `cabal` to start. The easiest way to manage GHC/Cabal versions at the moment is to use ghcup (https://www.haskell.org/ghcup/)
- Docker
- Some libraries
    * On Linux using apt-get, that will be : `sudo apt install rsync libz-dev libexpat1-dev liblmdb-dev liblzma-dev libgmp-dev`.
    * On MacOS using brew, that will be: `brew install rsync lmdb xz expat`.
    * On OpenBSD, see the dedicated section below -- it doesn't fit the ghcup-based flow above.

## OpenBSD

OpenBSD support is new and best-effort; nothing here has been validated on
real OpenBSD by anyone other than a first-time user following these exact
steps, so expect some iteration.

Two things make OpenBSD different enough to need its own path instead of
just following the Linux/macOS instructions above:

- **ghcup doesn't support OpenBSD** (no bindists), so GHC/cabal-install come
  from `pkg_add` instead. That means you get whatever GHC version the
  OpenBSD ports tree currently has, not the `ghc-9.10.3` pinned in
  `cabal.project`. If cabal complains about the compiler version, override
  the pin locally (don't edit the shared `cabal.project`):
  ```
  echo 'with-compiler: ghc-<installed-version>' > cabal.project.local
  ```
  A GHC version mismatch can also mean the exact dependency versions in
  `cabal.project.freeze` no longer resolve against that GHC's boot
  libraries; if so, `cabal build` will say which constraint fails, and it's
  usually a `cabal.project.local` `allow-newer`/constraint override rather
  than a real incompatibility.
- **The codebase uses Template Haskell** (via the `interpolate` package's
  `[i|...|]` quasiquotes, in ~10 modules) which requires GHC's internal
  bytecode interpreter to run *at compile time*. OpenBSD's strict W^X
  memory policy has historically been a source of bugs there. GHC 9.x
  carries OpenBSD-specific handling for this, but it's community-maintained
  rather than a GHC HQ tier-1 target -- if the build dies inside a TH splice
  with a memory-protection-flavored crash, this is why.
- **rsync**: OpenBSD's base `rsync` is `openrsync`, a separate
  implementation with a smaller flag set than GNU rsync. This project's
  rsync invocations (see `RPKI.Rsync`) use `--contimeout`, `--max-size`,
  `--min-size` and `--copy-links`, which may or may not all be supported.
  If repository fetches fail, install GNU rsync from ports (`pkg_add
  rsync`, installs to `/usr/local/bin/rsync`) and point rpki-prover at it
  with `--rsync-client-path=/usr/local/bin/rsync`.

Given that, the actual steps:

- Install dependencies via `pkg_add` (exact package names can drift between
  releases -- if these don't resolve, find the current ones with `pkg_info
  -Q ghc` / `pkg_info -Q cabal`):
  ```
  doas pkg_add ghc hs-cabal-install git gmake lmdb xz gmp rsync
  ```
- Clone the repo and run `./build-openbsd.sh`, which checks for the above
  tools and builds (it's `build-local.sh`'s logic plus the OpenBSD-specific
  preflight checks and notes above).
- `./scripts/smoke-test-openbsd.sh <path-to-binary>` runs `--version` /
  `--help` and checks for an rsync client, without needing network access to
  real RPKI repositories.
- `./package-openbsd.sh` builds and packages a release tarball -- see
  "Releasing" below.

For testing without dedicating a physical/cloud OpenBSD machine, use
`Vagrantfile` + `vagrant/provision-openbsd.sh` -- a local OpenBSD VM
(libvirt) for interactive build/debug work. Docker cannot run OpenBSD
guests, which is why this is a VM rather than a container.

**Box freshness matters more than you'd expect.** The obvious choice,
`generic/openbsd7`, is pinned to OpenBSD 7.4 across every version it's
published, and OpenBSD only keeps packages -- and, as it turns out,
install/upgrade sets -- live on its mirrors for the current and previous
release. A 7.4 box has no installable packages at all, and can't be fixed
with `sysupgrade` either: the one-hop-at-a-time upgrade needs 7.5's sets,
which are equally gone, and 7.4's `sysupgrade` binary predates the
`-R <version>` flag that could otherwise target a later release directly.
It's a dead end, not an inconvenience -- don't spend time re-attempting an
in-place upgrade if a box turns out to be this stale.

The `Vagrantfile` therefore defaults to `DefinedNet/openbsd78` instead
(real 7.8, native libvirt build, no upgrade needed). If that ever goes
stale or disappears, find a replacement the same way this one was found:
it needs a libvirt provider build, and to actually be a currently-supported
release -- check https://ftp.openbsd.org/pub/OpenBSD/ for which point
releases still have live packages before trusting a box's advertised
version, then `RPKI_OPENBSD_BOX=org/box-name vagrant up` (see the
`Vagrantfile` header for details) rather than editing the file.

## Build

- There's an annoying bug is the stack where `stack build rpki-prover:rpki-prover` always tries to build `rpki-prover-static` executable as well and, obviously, fails. This problem exists since forever
https://github.com/commercialhaskell/stack/issues/1406 and nobody gives a crap. That's why there is `package-template.yaml` that is being transformed to `package.yaml` depending on what kind of binary we want to build, normal or static. This can be fixed by moving to Cabal somewhere in the future.

So
- Full build is done by the `./build-local.sh` script, it should normally take 30-40 minutes on an average computer.
- Static binary is built by `./build-static.sh`.
- Tests can be run using `./run-tests.sh`.
- `ghcid` works just as is 
- For local docker image build use `./build-docker.sh`

## Bumping DB version

There is a value `currentDatabaseVersion` definted in `Database.hs`, that needs to be increased every time serialisation/deserialisation of the cache may break. In practice, that means whenever there's any change to the data types in `Domain.hs`. Changes in involved library types will also break serialisation, so essentially it's better to bump `currentDatabaseVersion` with any change of stack snapshot version of library version. Version change will result in prover erasing its cache and starting from scratch after upgrade and restart, which is a minor nuisance compared to processing serialisation errors.

## Releasing

At the moment releasing is done manually, since github actions consistently fail with out-of-memory errors.

For a release version `X.Y.Z` the procedure is this:

- Update version in the `package-template.yaml` file (TODO Make it automated?)
- `git tag -a vX.Y.Z -m "Release X.Y.Z"` 
- `git push -f --tags`
- Create and push docker image with `./docker-release.sh X.Y.Z`
- Build static Linux binary with `./build-static.sh`
- Build the OpenBSD tarball with `./package-openbsd.sh`, run on an OpenBSD
  host/VM.
- Create a release in github UI using `vX.Y.Z` tag and upload the the static binary and the OpenBSD tarball to the artifacts of the release.

