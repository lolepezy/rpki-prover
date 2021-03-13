# rpki-prover

Implementation of the RPKI relying party software (https://rpki.readthedocs.io/en/latest/tools.html) with the focus on a reasonable compromise between resource utilisation and ease of introducing changes.

Some of the decisions about the architecture and general flow are documented in ./doc/*.md files. Issues are tracked here https://github.com/lolepezy/rpki-prover/issues

Implemented features are

- Fetching from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- Output of VRPs in CSV and JSON formats
- Output of found problems
- Cache cleanup, scheduled revalidation, cache compaction, etc.
- Support for RTR protocol, both version 0 and 1
- Docker image
- Basic UI for reporting metrics and found problems

Current and future work
- Adjustment to the latest RFC 6486-bis: rrdp -> rsync fallback, "failed fetch" concept
- Static binaries (at least for linux)
- SLURM support

At the moment there are two options to use the program: building from sources and building a docker image. It is planed to have staticly-linked binaries for Linux in the future.

## Building from sources

The software is a daemon written in Haskell and can be built using [`stack`](https://docs.haskellstack.org/en/stable/README/).

The instruction below is for linux, but it can work equally for \*BSD or Mac (Windows support is not planned or tested).
    
   - Install libraries for: `lmdb`, `lzma`, `expat` and some others, like `gmp`.
      - On linux using apt-get, that will be : `sudo apt-get install libz-dev libexpat1-dev liblmdb-dev liblzma-dev libgmp-dev`.
      - On MacOS using brew, that will be: `brew install lmdb xz expat` 
      - It should be trivial to find the corresponding commands for other UNIX-like OSes or package managers
   - Install `stack` as described [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
   - Clone https://github.com/lolepezy/rpki-prover/
   - Run `stack install rpki-prover:rpki-prover` inside of the rpki-prover. It should take quite some time (30-50 minutes as it has to build all the required libraries)
   - Run `mkdirs.sh` script. It will create some directory structure inside of ~/.rpki and download TAL files from `https://github.com/NLnetLabs/routinator/tree/master/tals` (kudos, guys!)
   - Run `rpki-prover` from the `~/.local/bin`

Running Run `rpki-prover --help` gives some help on the CLI options.

Normally it prints quite a lot of logs about what it's doing to the stdout. After it prints "Validated all TAs, took ..." (it should take 2-4 minutes depending on how fast the CPU and network are) VRPs can be fetched by executing `curl -s http://localhost:9999/api/vrps.csv` (or `curl -s http://localhost:9999/api/vrps.json`).

Main page http://localhost:9999 is the UI that reports some metrics about trust anchorts, repositories and the list of errors.

## Use Docker image

Dockerfile is available and the image exposes port 9999 for HTTP API.

## Prometheus metrics 

Prometheus metrics are accessible via the standard `/metrics` path.

## Resource consumption

Cold start, i.e. the first start without cache takes about 2 minutes and consumer around 3 minutes of CPU time. This time can be reduced by setting higher `--cpu-count` value in case multiple CPUs are available. While CPU-intensive tasks scale pretty well (speed-up is sublinear up to 8-10 CPU cores), the total warm up time is moslty limited by the download time of the slowest of RPKI repositories and cannot be reduced drastically.

With default settings RPKI Prover consumes less than 1 hour of CPU time every 12 hours on a typical modern CPU.

The amount of memory needed for a smooth run for the current state of the repositories (5 trust anchors, ~220K of VRPs in total) is somewhere around 1.5GB. What can be confusing about memory usage is the figures given by top/htop.

An example of a server, running for a few weeks:
```
VIRT  RES    SHR
1.0T  4122M  2942M
```
Here `SHR` is largely domminated by the LMDB cache and other mmap-ed files (temporary files used to download RRDP repositories, etc.). That means that actual heap of the process is about `4122-2942=1180M`.

 ## Why Haskell?

- Relatively small code-base. Currently the size of it is around 10KLOC, including a lot of functionality implemented from scratch, such as CMS-parsing.
- Fast prototyping and smooth refactoring.
- Ease of introducing changes and very short time-to-market.
- Reasonable performance while the language is very high-level (GC, immutable data, powerful type system).
- Original motivation was "because it's cool", everything else came later.
