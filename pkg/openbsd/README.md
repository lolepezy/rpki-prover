# OpenBSD packaging

An OpenBSD port of `rpki-prover`, plus the scripts that build, sign and test a
package from it in a VM and turn the packages into a repository that
`pkg_add` can install from.

```
pkg/openbsd/
├── net/rpki-prover/        the port, laid out as in /usr/ports
│   ├── Makefile
│   ├── cabal.inc           MODCABAL_MANIFEST: every Hackage dependency, pinned
│   ├── distinfo            checksums for the tagged release (made by --from-tag)
│   ├── files/cabal.project replaces the upstream one in the build
│   └── pkg/                DESCR, PLIST, rc.d script, login class, README
├── Vagrantfile             the build VM (libvirt)
├── build-package.sh        host side: VM up, build, sign, test
├── build-repo.sh           host side: packages -> static pkg_add repository
└── guest/                  what build-package.sh runs inside the VM
    ├── prepare-vm.sh       disk, packages, ports tree
    ├── build-port.sh       make package
    └── test-package.sh     install / run / restart / remove checks
```

## Building

```sh
./pkg/openbsd/build-package.sh --test                     # 7.9, from HEAD
./pkg/openbsd/build-package.sh --release 8.0 --test       # once 8.0 is out
./pkg/openbsd/build-package.sh --from-tag --sign rpki-prover-pkg.sec --test
```

The package lands in `pkg/openbsd/packages/<release>/`. A build takes about
45 minutes on 8 CPUs, most of it for the ~300 Hackage dependencies; the ports
framework starts every build from a clean work directory. A new VM adds
another 15 minutes or so for the upgrade and setup.

By default the package is built from the committed `HEAD`. The script exports
`HEAD`, runs `generate-modules.sh` on the export, and puts the tarball where
the ports tree expects the release tarball. `make makesum` then records the
checksums in the VM's copy of the port only.

`--from-tag` fetches the GitHub tarball of tag `v<version>` instead, exactly as
an OpenBSD ports build would, and checks it against `distinfo`. The first
`--from-tag` build of a new version has no `distinfo` yet. It records one and
copies it back to `net/rpki-prover/distinfo`, to be looked over and
committed. Delete the old one when the version in `rpki-prover.cabal`
changes.

### Why a port, and why in a VM

A package has to be built on the release it is for: `libc` and every other
shared library carry a major version that changes between releases, so a 7.9
package does not install on 8.0. The ports framework is what OpenBSD uses to
build packages. It works out `WANTLIB` and the run dependencies, registers
the user, rc.d script, login class and `@sample` files in the packing list,
and checks all of it (`make port-lib-depends-check`, `user.list`). It also
leaves a port that could be submitted to ports@openbsd.org as it is.

GHC and cabal-install come from OpenBSD's own packages. OpenBSD 7.9 and 8.0
both ship GHC 9.10.3 and cabal-install 3.16/3.18, the versions this project
pins in `.tool-versions`, so the code builds unmodified. 7.8 ships GHC 9.8.3,
which lacks `GHC2024`, and is not targeted.

The newest OpenBSD Vagrant box is 7.8 (`DefinedNet/openbsd78`), so
`build-package.sh` sysupgrades the VM one release at a time to the target.
A release whose base sets are not published yet can't be reached that way
(8.0 had only packages on 2026-10-13); the script stops and says so. The box's
20G disk leaves 2.2G for `/usr/local`, and GHC alone takes 1.9G of that, so the
Vagrantfile adds a second 80G disk. `prepare-vm.sh` makes it `/usr/local` and
`/build`, and everything the ports tree writes goes to `/build`.

Traps that the scripts already handle:

- **datasize.** Without `PORTS_PRIVSEP` the ports build runs as root, with the
  limits of whoever ran `sudo` (1.5G for the `staff` class). GHC then fails on
  rpki-prover's own modules with "Unable to commit 1048576 bytes of memory".
  `build-port.sh` raises the limit. OpenBSD's own bulk builds run as `_pbuild`,
  whose class allows 12G.
- **Parallelism.** The port sets `DPB_PROPERTIES = parallel`; without it
  `MAKE_JOBS` is 1 and cabal builds the dependencies one at a time.
- **Hackage's CDN** occasionally breaks off a transfer to OpenBSD's `ftp`
  ("Invalid chunk size"). `build-port.sh` retries the fetch.

### Updating the dependencies

`cabal.inc` is generated from the plan that `cabal.project.freeze` produces:

```sh
# in the VM, in a copy of the source tree whose cabal.project has no
# source-repository-package (files/cabal.project has none either)
cabal build --dry-run exe:rpki-prover
cabal-bundler --openbsd rpki-prover --executable rpki-prover \
    --plan dist-newstyle/cache/plan.json -o cabal.inc
```

Regenerate it whenever the freeze file changes. The port does not use the
freeze file itself: the module adds every manifest entry as a local package,
so the manifest is what pins versions. The `hs-connection` fork in the
top-level `cabal.project` is left out on purpose, because nothing depends on
the `connection` package it provides.

## Publishing and installing

Packages are signed with signify. To make the signing key (keep the `.sec`
file out of the repository):

```sh
signify -G -n -p rpki-prover-pkg.pub -s rpki-prover-pkg.sec
```

`pkg_add` only trusts keys named `*pkg.pub` in `/etc/signify`, and finds the
key by the name recorded in the signature, so keep that file name.

`build-repo.sh` lays the signed packages out as a static directory that
`pkg_add` can read over HTTPS, for example on GitHub Pages next to the APT
repository:

```sh
./pkg/openbsd/build-repo.sh --pubkey rpki-prover-pkg.pub \
    --url https://lolepezy.github.io/rpki-prover/openbsd
```

The result is `pkg/openbsd/public/`: the public key,
`<release>/amd64/rpki-prover-<version>.tgz`, and an `index.html` per
directory, because `pkg_add` lists an HTTP directory by reading the links on
that page. Users then do, as root:

```sh
ftp -o /etc/signify/rpki-prover-pkg.pub https://lolepezy.github.io/rpki-prover/openbsd/rpki-prover-pkg.pub
export PKG_PATH=https://lolepezy.github.io/rpki-prover/openbsd/%v/%a/:installpath
pkg_add rpki-prover          # or pkg_add -u rpki-prover to upgrade
rcctl enable rpki_prover
rcctl start rpki_prover
```

`%v` and `%a` expand to the running release and architecture, and
`installpath` lets `pkg_add` take the dependencies (gmp, libffi, libiconv,
xz, rsync) from the official mirror in `/etc/installurl`. Without the key,
`pkg_add` refuses the package. All of this was tried against a local copy of
the repository served over HTTP. The installed package carries its own notes
in `/usr/local/share/doc/pkg-readmes/rpki-prover`.

## What the package installs

| Path | |
|---|---|
| `/usr/local/bin/rpki-prover` | the binary |
| `/etc/rc.d/rpki_prover` | rc.d script; not enabled on install |
| `/etc/login.conf.d/rpki_prover` | login class: datasize 8G, 4096 files |
| `/var/db/rpki-prover/` | state, owned by `_rpki-prover` (uid 906) |

The daemon's output goes to syslog as `rpki_prover` (daemon.info, so
`/var/log/daemon`). `pkg_delete` keeps `/var/db/rpki-prover` and the user,
and `pkg_delete -c` removes both. `pkg_add` prints a harmless `useradd`
warning that the home directory doesn't exist yet; the `@sample` line creates
it right after.

## OpenBSD specifics

- **No worker sandbox.** The Landlock sandbox is Linux-only, so workers log
  "Worker is not sandboxed". The OpenBSD counterpart would be
  `unveil(2)`/`pledge(2)` in the worker.
- **rsync, not openrsync.** Base `openrsync` rejects `--update`, and its usage
  lists none of `--max-size`, `--min-size` or `--copy-links`; `rpki-prover`
  passes all four. The package therefore depends on `net/rsync`, like
  `net/routinator`.
- **CPU count.** There is no `/proc` or `/sys`, so the physical core and
  cgroup detection finds nothing and `--cpu-count` defaults to the RTS `-N`,
  which is 2. On OpenBSD `getNumProcessors` would be accurate, since SMT is
  off by default. Resource accounting from `/proc/self` reads zeros.
- **Memory.** OpenBSD counts anonymous `mmap` against `datasize`, and the
  `daemon` class allows 4G per process. The login class raises that to 8G.
- **Disk.** State reaches a few GB under `/var/db` (1.7G after the first
  validation), and the installer's automatic layout gives `/var` only ~1G on
  small disks.
- **uid 906** is the next free uid in the ports `user.list` as of 2026-10.
  `build-port.sh` registers it locally. A port in the official tree would get
  its own uid.
