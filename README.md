# Contents 

* [Introduction](#introduction)
* [Features](#features)    
* [Usage](#usage)    
  - [Static Linux binary](#static-linux-binary)
  - [Docker image](#docker-image)
  - [Building from sources](#building-from-sources)
* [HTTP API](#http-api)
    - [Prometheus metrics](#prometheus-metrics)
* [Support of RSC](#support-of-rsc)
* [Resource consumption](#resource-consumption)
* [Why Haskell?](#why-haskell)


# Introduction <a name="introduction"></a>

RPKI prover is an implementation of the [RPKI relying party software](https://rpki.readthedocs.io/en/latest/rpki/using-rpki-data.html) with the focus on a reasonable compromise between resource utilisation and ease of introducing changes.

Issues are tracked [here](https://github.com/lolepezy/rpki-prover/issues), any questions can be asked there as well. 

This implementation seeks to address potential security vulnerabilites by utilising process isolation, memory and time constraints and other ways of preventing resource exhaustion attacks and make sure that "it keeps going" regardless of unstable or potentially maliciouly constructed RPKI repositories.

# Features <a name="features"></a>

- Fetching from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- UI for reporting metrics and found problems
- REST API for pretty much everything the validator does 
- Output of VRPs in CSV and JSON formats
- Support for RTR protocol, both version 0 and 1
- Support of SLURM (RFC 8416)
- Support of ASPA object validation and output
- Support of BGPSec certificates validation and RTR
- Support of RPKI Signed Checklists
- Support of RPKI Prefix Lists
- Static binaries for Linux
- Docker image

# Usage <a name="usage"></a>

Running `rpki-prover --help` gives some reasonable help on CLI options.

The only dependency needed for `rpki-prover` to run is `rsync` client.

`rpki-prover` is a daemon that runs periodic re-validation of all TAs in the RPKI hierachy. The results of these runs are exposes in UI, JSON API and Prometheus metrics. Also the `--with-rtr` option enables RTR server pushing VRP updates to RTR clients.

There is no config file and all the configuration is provided with CLI (most of the defaults are pretty reasonable, so normally you don't need to adjust a lot of parameters). Typical command line could look like this

```
/opt/bin/rpki-prover-linux.exe --rpki-root-directory /var/rpki/ --cpu-count 4 --http-api-port 8080 --log-level debug
```

There is an initialise step necessary to start after downloading or building the executable: you need to run something like `rpki-prover.exe --initialise --rpki-root-directory /var/where-you-want-data-to-be` to create the necessary FS layout in `/var/where-you-want-data-to-be`. It will download the TAL files to `/var/where-you-want-data-to-be/tals` as well.

## Static Linux binary <a name="static-linux-binary"></a>

Every [release](https://github.com/lolepezy/rpki-prover/releases) includes statically linked Linux x64 executable, just download and run it. 

## Docker image <a name="docker-image"></a>

It is possible to run rpki-prover as `docker run lolepezy/rpki-prover:latest`. The image is available on Docker Hub.

It is also possible to build your own image using `docker build . --file Dockerfile.prover --tag rpki-prover`.

Since `rpki-prover` needs to have some persistent directory to use for TALs, caches, temporary files, etc. (the aforementioned `/var/where-you-want-data-to-be`), there needs to be a persistent volume configured for it, so typical sequence of commands could be something like this

```
docker volume create rpki-data
docker pull lolepezy/rpki-prover:latest
docker run --mount source=rpki-data,target=/rpki-data lolepezy/rpki-prover:latest --initialise
docker run -p 9999:9999 --mount source=rpki-data,target=/rpki-data lolepezy/rpki-prover:latest --cpu-count 4 --revalidation-interval 300
``` 
The important part here is `target=/rpki-data`, this directory is created by default inside of the docker container. Otherwise it can be adjusted as in
```
docker run -p 9999:9999 --mount source=rpki-data,target=/something-else lolepezy/rpki-prover:latest --rpki-root-directory /something-else
```

## Building from sources <a name="building-from-sources"></a>

The software is a daemon written in Haskell and can be built using [`stack`](https://docs.haskellstack.org/en/stable/README/).

The instruction below is for linux, but it can work equally for \*BSD or Mac (Windows support is not planned or tested).

   - The prerequisites are a few libraries (`lmdb`, `lzma`, `expat` and `gmp`) and the `rsync` client. It can be done    
      - On Linux using apt-get, that will be : `sudo apt-get install rsync libz-dev libexpat1-dev liblmdb-dev liblzma-dev libgmp-dev`.
      - On MacOS using brew, that will be: `brew install rsync lmdb xz expat`.
      - It should be trivial to find the corresponding commands for other UNIX-like OSes or package managers.

   - Install `stack` as described [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

   - Clone https://github.com/lolepezy/rpki-prover/

   - Run `./build-local.sh` inside of the `rpki-prover` directory. It should take quite some time (30-50 minutes as it has to build all the required libraries)

   - Run `rpki-prover` from the `~/.local/bin` when repeating steps from the usage section above.

Normally it prints quite a lot of logs about what it's doing to the stdout. After it prints "Validated all TAs, took ..." (it should take 2-4 minutes depending on how fast the CPU and network are) VRPs can be fetched by executing `curl -s http://localhost:9999/api/vrps.csv` (or `curl -s http://localhost:9999/api/vrps.json`).

Main page http://localhost:9999 is the UI that reports some metrics about trust anchorts, repositories and the list of errors and warnings.


# HTTP API <a name="http-api"></a>

There are a bunch of API endpoints. The easiest way to find out what is available is to go to the `/swagger-ui` URL and explore the Swager UI. 

## Prometheus metrics <a name="prometheus-metrics"></a>

Prometheus metrics are accessible via the standard `/metrics` path.


# Support of RSC <a name="support-of-rsc"></a>

RPKI prover supports validating RPKI Signed Checklists (https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/).

In order to validate a set of files with an RSC object it is necessary to have a running rpki-prover instance to be able to use its cache of validated object. In the examples below it is assumed that there's an instance of rpki-prover (the same version) running with `/var/prover` set as `--rpki-root-directory` option. It is also possible to skip `--rpki-root-directory` parameter assuming that the default (`~/.rpki`) with be used.

The following example validates two files `foo.txt` and `bar.bin` against the `checklist.sig` object:

```
rpki-prover  --rpki-root-directory /var/prover --verify-signature --signature-file checklist.sig --verify-files foo.txt bar.bin
```

The following example validates all files in the `dir` directory against the `checklist.sig` object:

```
rpki-prover  --rpki-root-directory /var/prover --verify-signature --signature-file checklist.sig --verify-directory ./dir
```


# Resource consumption <a name="resource-consumption"></a>

Cold start, i.e. the first start without cache takes at least 2 minutes and consumes around 3 minutes of CPU time. This time can be slightly reduced by setting higher `--cpu-count` value in case multiple CPUs are available. While CPU-intensive tasks scale pretty well (speed-up is sublinear up to 8-10 CPU cores), the total warm up time is moslty limited by the download time of the slowest of RPKI repositories and cannot be reduced drastically. 

After initial warmup, it's not a very CPU-bound application. With default settings RPKI Prover consumes about 1 hour of CPU time every 18 hours on a typical modern CPU, creating load average of 5-10%. Smaller revalidation interval will increase the load.

The amount of memory needed for a smooth run for the current state of the repositories (6 trust anchors, including [AS0 TA](https://www.apnic.net/community/security/resource-certification/tal-archive/) of APNIC with about 330K of VRPs in total) is somewhere around 1.5-2GB for all processes in total. Adding or removing TAs can increase or reduce this amount. What can be confusing about memory usage is the figures given by `top/htop`.

An example of a server, running for a few days:
```
VIRT  RES    SHR
1.0T  4463M  3920M
```
Here `SHR` is largely dominated by the LMDB cache and other mmap-ed files (temporary files used to download RRDP repositories, etc.). That means that actual heap of the process is about `4463-3920=543M`. 

Every validation or repository fetch runs as a separate process with its own heap, with typical heap size for the validator up to 600-700M and up to 100-200MB for a fetching process.

Note that memory consumption is mostly determined by how big the biggest objects are and not that much by how many there are objects in total, so the growth of repositories is not such a big issue for rpki-prover. It it recommended to have 3GB of RAM available on the machine mostly to reduce the IOPS related to reading objects from the LMDB cache. Since every validation typically goes through 230K of objects (at the moment of writing), each of them being 3Kb in size on average, it would be benificial to have at least few hundred of megabytes in FS page cache.

Disk space usage depends on the `--cache-lifetime-hours` parameter. The default is 72 hours and it results in a cache size about 2Gb. 72 hours is a little bit on a big side, so lower values would reduce the amount of data stored. However, LMDB is not very good in reusing the free space in its file, so physical size of the `cache` directory can be 2 or more times bigger than the total size of data in it. There is a compaction procedure that kicks in when the LMDB file size is 2 or more times bigger than the total size of all data. So overall, in the worst case scenario, it would need approximately 1GB of disk space for every 10 hours of `--cache-lifetime-hours`.

# Known issues <a name="known-issues"></a>

 - From time to time a message 'rpki-prover: Thread killed by timeout manager' may be printed to `stderr`. It's the result of a bug in the HTTP server used for API and UI and is harmless. It will be fixed one way or the other in future versions.
 - As mentioned before, total RSS of the process can go up to several gigabytes even though most of it mapped to LMDB cache and not in RAM. It may, however, be that `rpki-prover` is killed by OOM and some configuration adjustments would be needed to prevent it.

# Why Haskell? <a name="why-haskell"></a>

- Relatively small code-base. Currently the size of it is around 10KLOC, including a lot of functionality implemented from scratch, such as CMS-parsing.
- Fast prototyping and smooth refactoring.
- Ease of introducing changes and very short time-to-market.
- Reasonable performance while the language is very high-level (GC, immutable data, powerful type system).
- Original motivation was "because it's cool", everything else came later.
