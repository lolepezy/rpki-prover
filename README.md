# Contents

* [Introduction](#introduction)

* [Features](#features)

* [Installation and usage](#usage)
  * [Static Linux binary](#static-linux-binary)
  * [OpenBSD binary](#openbsd-binary)
  * [Docker image](#docker-image)
  * [Building from source](#building-from-source)

* [Functionality](#functionality)
  * [RTR server](#rtr-server)
  * [HTTP API](#http-api)
  * [Prometheus metrics](#prometheus-metrics)  
  * [Support for RSC](#support-for-rsc)
  * [Resource consumption](#resource-consumption)
  * [Known issues](#known-issues)
  
* [Why Haskell?](#why-haskell)

# Introduction <a name="introduction"></a>

RPKI Prover is [RPKI relying party software](https://rpki.readthedocs.io/en/latest/rpki/using-rpki-data.html) designed to balance resource efficiency, operational robustness, and maintainability. It uses process isolation, memory and time limits, and related safeguards to reduce the impact of resource-exhaustion attacks and keep validation running even when repositories are unstable or intentionally malformed.

Issues are tracked [here](https://github.com/lolepezy/rpki-prover/issues). You can also ask questions there.

# Features <a name="features"></a>

* UI for reporting metrics and problems
* REST API covering most validation and reporting operations
* Output of VRPs in CSV and JSON formats
* RTR server supporting versions 0 and 1
* Implements RFC 8360 validation reconsidered behavior
* Support for SLURM (RFC 8416)
* Validates ASPA objects and exposes them in output
* Support for BGPSec certificate validation and RTR
* Support for RPKI Signed Checklists
* Support for RPKI Prefix Lists
* Static binaries for Linux
* OpenBSD binary release
* Docker image

# Installation and usage <a name="usage"></a>

Running `rpki-prover --help` displays a description of CLI options.

For a prebuilt `rpki-prover` binary, the only external runtime dependency is an `rsync` client.

`rpki-prover` is a daemon that periodically revalidates all TAs in the RPKI hierarchy. The results are exposed via the UI, JSON API, and Prometheus metrics. The `--with-rtr` option enables the RTR server, which pushes VRP updates to RTR clients.

There is no configuration file; all configuration is provided via the CLI. Most defaults are reasonable, so you typically do not need to adjust many parameters. By default, the HTTP API listens on port `9999`. A typical command line might look like this:

```
/opt/bin/rpki-prover --rpki-root-directory /var/rpki/ --cpu-count 4 --http-api-port 9999
```

At the first launch, `rpki-prover` initializes the filesystem layout and
downloads [TAL files](https://www.ripe.net/manage-ips-and-asns/resource-management/rpki/ripe-ncc-rpki-trust-anchor-structure/).

## Static Linux binary <a name="static-linux-binary"></a>

Every [release](https://github.com/lolepezy/rpki-prover/releases) includes a statically linked Linux x64 executable. Just download and run it.

## OpenBSD binary <a name="openbsd-binary"></a>

Releases also include an OpenBSD tarball. Unlike the Linux binary this one is
*dynamically* linked -- OpenBSD ports generally don't ship static archives to
link against, so a fully static binary isn't practical there. Install the
runtime libraries first (`doas pkg_add lmdb xz gmp`), then unpack and run.

`rpki-prover` also needs an `rsync` client. OpenBSD's base `rsync` is
`openrsync`, which supports fewer flags than GNU rsync; if repository
fetches fail, `doas pkg_add rsync` and pass
`--rsync-client-path=/usr/local/bin/rsync`.

## Docker image <a name="docker-image"></a>

You can run `rpki-prover` using Docker. The image is available on Docker Hub.

Since `rpki-prover` needs a persistent directory for TALs, caches, and temporary files, a volume-backed run is recommended. A typical sequence of commands looks like this:

```
docker volume create rpki-data
docker pull lolepezy/rpki-prover:latest
docker run -p 9999:9999 --mount source=rpki-data,target=/rpki-data lolepezy/rpki-prover:latest
```

The important part is `target=/rpki-data`, which matches the default root directory inside the container. Alternatively:

```
docker run -p 9999:9999 --mount source=rpki-data,target=/something-else lolepezy/rpki-prover:latest --rpki-root-directory /something-else
```

## Building from source <a name="building-from-source"></a>

The software can be built using [`cabal`](https://www.haskell.org/cabal/). The easiest way to manage GHC/Cabal versions at the moment is to use [`ghcup`](https://www.haskell.org/ghcup/).

The instructions below are for Linux but apply equally to \\\*BSD and macOS. Windows is not supported or tested.

* Install prerequisites: `lmdb`, `lzma`, `expat`, `gmp`, and `rsync`.

  * On Debian-based Linux:

    ```
    sudo apt-get install rsync libz-dev libexpat1-dev liblmdb-dev liblzma-dev libgmp-dev pkg-config
    ```
    (on RPM-based -- their respective `yum/dnf` versions)
  * On macOS:

    ```
    brew install rsync lmdb xz expat
    ```
  * On OpenBSD, see [DEVELOPER.md](DEVELOPER.md#openbsd) -- it needs a
    different path since `ghcup` doesn't support OpenBSD, and there's a
    caveat about `rsync` flag compatibility. There's also a
    [`build-openbsd.sh`](build-openbsd.sh) script and a `Vagrantfile` for
    testing in a local VM, neither of which is covered by the generic
    instructions below.
  * For other Unix-like OSes, use the appropriate package manager.

* Install `GHC` and `Cabal` using [`ghcup`](https://www.haskell.org/ghcup/) (not available on OpenBSD -- see above):

* Clone the repository:

  ```
  git clone https://github.com/lolepezy/rpki-prover/
  cd rpki-prover
  ```

* Install `GHC` and `Cabal` using [`ghcup`](https://www.haskell.org/ghcup/):

  ```
  ghcup run --install
  ```

  This reads the required versions from `.tool-versions` in the repository root and installs them if missing.

* Run `./build-local.sh` in the `rpki-prover` directory. This takes 30-50 minutes because it builds the required libraries.

* After building, run `rpki-prover` from `~/.local/bin` as described in the usage section.

It prints logs to stdout. Once you see:

```
Validated all TAs, took ...
```

(which may take several minutes), you can fetch VRPs:

```
curl -s http://localhost:9999/api/vrps.csv
curl -s http://localhost:9999/api/vrps.json
```

# Functionality

## RTR server <a name="rtr-server"></a>

RPKI Prover can also run as an [RTR server](https://www.rfc-editor.org/rfc/rfc8210). Enable it with the `--with-rtr` option. In most deployments, you will also want to set `--rtr-address` and `--rtr-port`.

## HTTP API <a name="http-api"></a>

Visit [http://localhost:9999](http://localhost:9999) to view the UI reporting metrics, trust anchors, repositories, and any errors or warnings.

There are multiple API endpoints. The easiest way to explore them is via the `/swagger-ui` URL using Swagger UI.

## Prometheus metrics <a name="prometheus-metrics"></a>

Prometheus metrics are available at the standard `/metrics` path.

## Support for RSC <a name="support-for-rsc"></a>

RPKI Prover supports validating RPKI Signed Checklists ([RSC draft](https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/)).

To validate files using an RSC object, you need a running `rpki-prover` instance (same version) to use its cache of validated objects. In the examples below, it is assumed that the instance is running with `--rpki-root-directory /var/prover`. You may omit this option to use the default (`~/.rpki`).

To validate `foo.txt` and `bar.bin` against `checklist.sig`:

```
rpki-prover --rpki-root-directory /var/prover --verify-signature --signature-file checklist.sig --verify-files foo.txt bar.bin
```

To validate all files in a directory:

```
rpki-prover --rpki-root-directory /var/prover --verify-signature --signature-file checklist.sig --verify-directory ./dir
```

## Resource consumption <a name="resource-consumption"></a>

A cold start (no cache) takes at least a few minutes and consumes 3-5 minutes of CPU time. This can be slightly reduced by increasing `--cpu-count` if multiple CPUs are available. CPU-bound tasks scale well up to 8–10 cores, but overall warm-up time is often limited by the slowest RPKI repository. After warm-up, the application is not very CPU-intensive. 

Current memory usage for 7 trust anchors (including [AS0 TA](https://www.apnic.net/community/security/resource-certification/tal-archive/) and [AS0 of LACNIC](https://www.lacnic.net/4984/2/lacnic/rpki-rpki-trust-anchor)) with around 800K VRPs is under 3GB total. Adding or removing TAs affects this.

Memory reporting in tools like `top/htop` can be misleading. For example:

```
VIRT   RES    SHR
1.0T   4463M  3920M
```

Here, `SHR` is mostly LMDB cache and mmap-ed files. So actual heap usage is around `4463 - 3920 = 543M`.

Memory consumption is determined more by the size of the largest objects than by their count. 3GB RAM is recommended to reduce IOPS during validation. Each validation goes through over 400K objects, \~3KB each, so several hundred MB in FS page cache is beneficial.

Disk usage depends on `--cache-lifetime-hours`. The default of 24 hours results in a \~4GB cache. Periodic compaction requires an extra 30–40% of disk space. Around 2GB may also be needed for `rsync` mirrors. A total of 10GB is recommended.

# Known issues <a name="known-issues"></a>

* As noted, total RSS can reach several GB, mostly mapped to the LMDB cache. However, `rpki-prover` may be killed by the OOM killer unless properly configured.

# Why Haskell? <a name="why-haskell"></a>

* Compact codebase (\~20KLOC), including custom CMS parsing
* Fast prototyping and smooth refactoring
* Quick iteration and short time-to-market
* High-level language with good performance (GC, immutability, powerful type system)
* Initially chosen "because it's cool"—the rest followed
