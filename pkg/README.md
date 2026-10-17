# Debian/Ubuntu and RHEL packaging for rpki-prover

Everything needed to build, test and publish `.deb` packages of `rpki-prover`
for the current Debian and Ubuntu LTS releases, and `.rpm` packages for
RHEL 9 and 10 and their rebuilds (see [RPM packages](#rpm-packages)).

```
pkg/
├── releases.conf                 target releases, versions and base images
├── Dockerfile.deb-builder        per-release build environment (ghcup + cabal)
├── build-packages.sh             host side: build a package for each release
├── build-deb-in-container.sh     container side: what actually runs the build
├── debian/                       the debian/ directory, as templates
├── rpm-releases.conf             the same for the RPM targets
├── Dockerfile.rpm-builder        per-release RPM build environment
├── build-rpm-packages.sh         host side of the RPM build
├── build-rpm-in-container.sh     container side of the RPM build
├── rpm/                          the spec file and what it installs
├── test-scripts/
│   ├── test-rpki-prover.sh       install / upgrade / remove / purge checks
│   ├── test-rpki-prover-rpm.sh   install / upgrade / remove checks for the .rpm
│   ├── test-worker-sandbox.sh    worker processes and their Landlock sandbox
│   ├── Dockerfile.systemd        a Debian container that runs systemd
│   ├── Dockerfile.systemd-rpm    an EL container that runs systemd
│   └── run-in-docker.sh          fast test loop in those containers
├── vagrant/Vagrantfile           the same checks in real VMs, for releases
└── apt-repo/build-apt-repo.sh    turns the packages into a signed APT repo
```

## Building

```sh
./pkg/build-packages.sh                  # every release in pkg/releases.conf
./pkg/build-packages.sh bookworm         # just one
```

Packages end up in `pkg/packages/<codename>/`.

For each release this builds a container image from that release's base image
and runs `dpkg-buildpackage` inside it. Building in the target release's own
container is the point of the exercise: the binary is then linked against that
release's `libc`, `libgmp`, `liblzma` and so on, and `dpkg-shlibdeps` derives
a `Depends:` line that is correct for it. A single binary built once and
shipped to every release would either over-constrain or silently under-declare
those dependencies.

GHC and cabal come from ghcup at the versions pinned in `.tool-versions`, not
from Debian, since the versions this project needs are not the ones Debian
ships. The build image pre-compiles every Haskell dependency into a Docker
layer keyed on `rpki-prover.cabal`, `cabal.project` and
`cabal.project.freeze`, so editing sources does not trigger a rebuild of the
whole dependency set. The first build of an image still takes a long while.

## What the package installs

| Path | |
|---|---|
| `/usr/bin/rpki-prover` | the binary |
| `/lib/systemd/system/rpki-prover.service` | hardened unit, enabled and started on install |
| `/etc/default/rpki-prover` | conffile holding the command line options |
| `/usr/share/man/man1/rpki-prover.1.gz` | man page, generated from `--help` |
| `/var/lib/rpki-prover/` | state: object cache, rsync mirrors, TALs |

The service runs as the `rpki-prover` system user, created by the postinst.
`rpki-prover` has no configuration file, so `/etc/default/rpki-prover` holds a
single `RPKI_PROVER_ARGS` variable that the unit expands into the command
line; `--rpki-root-directory` is passed by the unit itself and must not be
repeated there.

On its first start the daemon finds `/var/lib/rpki-prover/tals` empty and
downloads the five RIR trust anchor locators from the RIRs, which is its
normal upstream behaviour. Debian's `rpki-trust-anchors` package is therefore
only a `Suggests:`; `/etc/default/rpki-prover` documents how to use it
instead. Note that setting `--extra-tals-directory` switches the automatic
download off.

Removing the package keeps `/etc/default/rpki-prover` and
`/var/lib/rpki-prover`, so reinstalling does not force a cold start. Purging
removes both along with the system user.

`pkg/debian/rpki-prover.lintian-overrides` suppresses two tags that cannot be
fixed and should not be: `hardening-no-pie`, because GHC cannot link a PIE
against a cabal store that is not uniformly PIC, and
`initial-upload-closes-no-bugs`, because there is no ITP bug for a package
that is not going through the Debian archive. Anything else lintian reports is
worth looking at.

### Why debhelper rather than assembling the .deb by hand

`dpkg-deb --build` over a staged tree would be fewer moving parts, but
`dh_shlibdeps` computing the shared library dependencies per release,
`dh_installsystemd` generating the correct enable/start/restart snippets, and
`dh_installdeb` registering the conffile are all things that are easy to get
subtly wrong by hand and that differ between releases. The `debian/` directory
here is templated (`control`, `rules`, maintainer scripts and the unit are
static; only `changelog` is generated) and copied into place by the build
script, so the repository root stays clean.

## Testing

Two levels, both driving the same `test-scripts/test-rpki-prover.sh`, which
checks a fresh install, an upgrade over an older package with a locally
modified conffile, `apt-get remove` and `apt-get purge`, and that the service
comes up, listens, loads TALs and does not restart-loop.

Fast loop, in a systemd-enabled container:

```sh
./pkg/test-scripts/run-in-docker.sh bookworm
```

Before tagging a release, in real VMs:

```sh
cd pkg/vagrant
vagrant up bookworm      # runs the tests as part of provisioning
vagrant destroy -f bookworm
```

Drop the previous release's package into `pkg/packages/<codename>/old/` to
have the upgrade path exercised too; without it that part is skipped.

## Releasing

Tagging `vX.Y.Z` runs `.github/workflows/debian-packages.yml`, which builds
and tests a package per release and attaches them to a draft GitHub release.

Package versions follow the Debian backports convention so that the same
upstream version orders correctly across suites:

```
0.10.1-1~deb12u1  <  0.10.1-1~deb13u1  <  0.10.1-1
```

(bookworm, trixie, sid). Ubuntu releases use the analogous
`~ubuntu22.04u1` / `~ubuntu24.04u1` (jammy, noble) -- only LTS releases are
targeted, since this is a long-running daemon rather than something users
reinstall every six months. The mapping lives in `pkg/releases.conf`, which
is the single source of truth for the build script, the container test
harness and the CI matrix. Adding a release means adding one line there.

## Publishing an APT repository

`pkg/apt-repo/build-apt-repo.sh` turns `pkg/packages/` into a signed, plain
static APT repository:

```sh
./pkg/apt-repo/build-apt-repo.sh --key <gpg-key-id> --url https://lolepezy.github.io/rpki-prover
```

It writes `pool/`, `dists/<codename>/` with signed `Release`/`InRelease`, the
armoured public key and an `index.html` with copy-pasteable setup
instructions, all under `pkg/apt-repo/public/`.

The `publish` job in the workflow uploads that to GitHub Pages. It is opt-in
(run the workflow manually with `publish: true`) and needs two secrets:

- `APT_REPO_GPG_KEY` — the armoured private signing key
- `APT_REPO_GPG_KEY_ID` — the key id to sign with

Use a signing key created for this purpose only, not a personal key.

GitHub Pages is free and entirely under your control, which is why it is
wired up here. Other free options for open source projects, if you would
rather not run the signing yourself:

- **openSUSE Build Service** — builds and hosts Debian and RPM packages for
  open source projects, and does the repository signing for you. The closest
  thing to a real distribution archive.
- **Cloudsmith** and **packagecloud.io** — both have free open source tiers
  and hosted, signed APT repositories.
- **Launchpad PPAs** are Ubuntu-only, so not useful here.

Whichever you pick, the packages this directory produces are the input; only
the last step changes.

## RPM packages

```sh
./pkg/build-rpm-packages.sh              # every release in pkg/rpm-releases.conf
./pkg/build-rpm-packages.sh el9          # just one
```

This works the same way as the `.deb` build: one container per release from
`pkg/rpm-releases.conf` (`el9`, `el10`, both on Rocky Linux images), GHC and
cabal from ghcup, dependencies pre-built into the image, and `rpmbuild -bb
--build-in-place` over a copy of the checkout. Packages end up in
`pkg/packages/el9/` and `pkg/packages/el10/`, named like
`rpki-prover-0.11.0-1.el9.x86_64.rpm`. The `.el9`/`.el10` tag comes from the
build image's `%{dist}`, so one package per major version serves RHEL, Rocky,
AlmaLinux and Oracle Linux alike. Shared library requirements are generated
by rpmbuild from the binary, the counterpart of `dpkg-shlibdeps`.

`pkg/rpm/rpki-prover.spec` installs the same things as the `.deb`, except:

| `.deb` | `.rpm` |
|---|---|
| `/etc/default/rpki-prover` | `/etc/sysconfig/rpki-prover`, `%config(noreplace)` |
| `/lib/systemd/system/rpki-prover.service` | `/usr/lib/systemd/system/rpki-prover.service`, generated from the same file with the `EnvironmentFile=` path changed |
| user created by the postinst | `/usr/lib/sysusers.d/rpki-prover.conf`, plus a `%pre` for EL9's rpm, which does not apply sysusers files itself |
| service enabled and started on install | service left disabled, as `%systemd_post` defers to the presets; `systemctl enable --now rpki-prover` |
| `remove` keeps config and state, `purge` deletes both and the user | there is no purge: erasing keeps the user and `/var/lib/rpki-prover`, deletes an unmodified config file and keeps a modified one as `.rpmsave` |

Upgrades restart a running service (`%systemd_postun_with_restart`), and a
locally edited `/etc/sysconfig/rpki-prover` is left alone; if the packaged one
changed too, the new one lands next to it as `.rpmnew`.

The tests are `test-scripts/test-rpki-prover-rpm.sh`, which mirrors
`test-rpki-prover.sh` but checks the RPM behaviour above. Its `cleanup` mode
does by hand what a purge would, so that the upgrade test starts from a clean
machine. Both test loops pick the RPM path for the `el*` names:

```sh
./pkg/test-scripts/run-in-docker.sh el9
./pkg/test-scripts/run-in-docker.sh --sandbox el10

cd pkg/vagrant
vagrant up el9           # Rocky Linux 9; RPKI_PROVER_BOX_EL9=almalinux/9 for Alma
vagrant destroy -f el9
```

For the upgrade test, drop the previous release's `.rpm` into
`pkg/packages/<release>/old/`. To exercise the upgrade path without one, build
the same sources twice with different revisions:

```sh
./pkg/build-rpm-packages.sh --output /tmp/old el9 && mkdir -p pkg/packages/el9/old \
  && mv /tmp/old/el9/*.rpm pkg/packages/el9/old/
./pkg/build-rpm-packages.sh --revision 2 el9
```
