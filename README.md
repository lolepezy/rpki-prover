# rpki-prover

Implementation of the [RPKI relying party software](https://rpki.readthedocs.io/en/latest/rpki/using-rpki-data.html) with the focus on a reasonable compromise between resource utilisation and ease of introducing changes.

Some of the decisions about the architecture and general flow are documented in ./doc/*.md files. Issues are tracked [here](https://github.com/lolepezy/rpki-prover/issues), any questions can be asked there as well. 

Implemented features are

- Fetching from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- Output of VRPs in CSV and JSON formats
- Output of found problems
- Cache cleanup, scheduled revalidation, cache compaction, etc.
- Support for RTR protocol, both version 0 and 1
- Basic UI for reporting metrics and found problems
- Adjustment to the latest RFC 6486-bis: rrdp -> rsync fallback, "failed fetch" concept
- Static binaries for Linux
- Docker image

Current and future work
- SLURM support
- History of validations in UI
- Ergonomics for long-running processes (automatic cleanups, cache DB migration, etc.)
- CPU and memory optimisations
 


# Using rpki-prover

`rpki-prover` is a daemon that runs periodic re-validation of all TA in the RPKI hierachy. The results of these runs are exposes in UI, JSON API and Prometheus metrics. Also the option `--with-rtr` enable RTR server that sends VRP updates to the RTR clients.

There is no config file and all the configuration is provided with CLI (most of the defaults are pretty reasonable, so normally you don't need to adjust a lot of parameters). Running `rpki-prover --help` gives some help on the CLI options.

There is an initialise step necessary to start after downloading or building the executable: you need to run something like `rpki-prover.exe --initialise --rpki-root-directory /var/where-you-want-data-to-be` to create the necessary FS layout in `/var/where-you-want-data-to-be`. It will download the TAL files to `/var/where-you-want-data-to-be/tals` as well. The awkward part related to ARIN TAL license agreement is pretty much a rip off from the Routinator implementation as the most convenient for the user.

## Static Linux binary

Every [release](https://github.com/lolepezy/rpki-prover/releases) includes a statically linked Linux x64 executable, just download and run it. 

## Docker image

It's possible to run rpki-prover as `docker run lolepezy/rpki-prover:latest`. The image is available on Docker Hub and it's about 80mb in size.

It's also possible to build your own image using `docker build . --file Dockerfile.prover --tag rpki-prover`.

## Building from sources

The software is a daemon written in Haskell and can be built using [`stack`](https://docs.haskellstack.org/en/stable/README/).

The instruction below is for linux, but it can work equally for \*BSD or Mac (Windows support is not planned or tested).

   - The prerequisites are a few libraries (`lmdb`, `lzma`, `expat` and `gmp`) and the `rsync` client. It can be done    
      - On Linux using apt-get, that will be : `sudo apt-get install rsync libz-dev libexpat1-dev liblmdb-dev liblzma-dev libgmp-dev`.
      - On MacOS using brew, that will be: `brew install rsync lmdb xz expat`.
      - It should be trivial to find the corresponding commands for other UNIX-like OSes or package managers.

   - Install `stack` as described [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

   - Clone https://github.com/lolepezy/rpki-prover/

   - Run `stack install rpki-prover:rpki-prover` inside of the `rpki-prover` directory. It should take quite some time (30-50 minutes as it has to build all the required libraries)

   - Run `mkdirs.sh` script. This script accepts an argument -- directory where `rpki-prover` will store the TAL files, cache, rsync mirror and different temporary files. If the argument is not set the default `$HOME/.rpki` will be used. After creating directory structure it will download TAL files from `https://github.com/NLnetLabs/routinator/tree/master/tals` (kudos, guys!)

   - Run `rpki-prover` from the `~/.local/bin`

Normally it prints quite a lot of logs about what it's doing to the stdout. After it prints "Validated all TAs, took ..." (it should take 2-4 minutes depending on how fast the CPU and network are) VRPs can be fetched by executing `curl -s http://localhost:9999/api/vrps.csv` (or `curl -s http://localhost:9999/api/vrps.json`).

Main page http://localhost:9999 is the UI that reports some metrics about trust anchorts, repositories and the list of errors and warnings.


## Prometheus metrics 

Prometheus metrics are accessible via the standard `/metrics` path.

## Resource consumption

Cold start, i.e. the first start without cache takes about 2 minutes and consumer around 3 minutes of CPU time. This time can be reduced by setting higher `--cpu-count` value in case multiple CPUs are available. While CPU-intensive tasks scale pretty well (speed-up is sublinear up to 8-10 CPU cores), the total warm up time is moslty limited by the download time of the slowest of RPKI repositories and cannot be reduced drastically.

With default settings RPKI Prover consumes less than 1 hour of CPU time every 12 hours on a typical modern CPU.

The amount of memory needed for a smooth run for the current state of the repositories (5 trust anchors, ~220K of VRPs in total) is somewhere around 1.5GB. Adding [AS0 trust anchor](https://www.apnic.net/community/security/resource-certification/tal-archive/), can make increase it to 2GB or so.  What can be confusing about memory usage is the figures given by `top/htop`.

An example of a server, running for a few weeks:
```
VIRT  RES    SHR
1.0T  4122M  2942M
```
Here `SHR` is largely dominated by the LMDB cache and other mmap-ed files (temporary files used to download RRDP repositories, etc.). That means that actual heap of the process is about `4122-2942=1180M`.

Disk space usage depends on the `--cache-lifetime-hours` parameter. The default is 72 hours and it results in a cache size about 2Gb. 72 hours is a little bit on a big side, so lower values would reduce the amount of data stored. However, LMDB is not very good in reusing the free space in its file, so physical size of the `cache` directory can be 2 or more times bigger than the total size of data in it. There is a compaction procedure that kicks in when the LMDB file size is 3 or more times bigger than the total size of all data. So overall, in the worst case scenario, it would need approximately 1GB of disk space for every 10 hours of `--cache-lifetime-hours`.

 ## Why Haskell?

- Relatively small code-base. Currently the size of it is around 10KLOC, including a lot of functionality implemented from scratch, such as CMS-parsing.
- Fast prototyping and smooth refactoring.
- Ease of introducing changes and very short time-to-market.
- Reasonable performance while the language is very high-level (GC, immutable data, powerful type system).
- Original motivation was "because it's cool", everything else came later.
