# rpki-prover

Implementation of the [RPKI relying party software](https://rpki.readthedocs.io/en/latest/rpki/using-rpki-data.html) with the focus on a reasonable compromise between resource utilisation and ease of introducing changes.

Issues are tracked [here](https://github.com/lolepezy/rpki-prover/issues), any questions can be asked there as well. 

Implemented features are

- Fetching from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- Adjustment to the latest RFC 6486-bis: rrdp -> rsync fallback, "failed fetch" concept
- Basic UI for reporting metrics and found problems
- Output of VRPs in CSV and JSON formats
- Support of SLURM (RFC 8416)
- Cache cleanup, scheduled revalidation, cache compaction, so it can run unlimited time without draining resources
- Support for RTR protocol, both version 0 and 1
- Static binaries for Linux
- Docker image.

Current and future work
- History of validations in UI
- Ergonomics for long-running processes (more subtle automatic cleanups, cache DB migration, etc.)
- CPU and memory optimisations 


# Using rpki-prover

Running `rpki-prover --help` gives some reasonable help on CLI options.

The only dependency needed for `rpki-prover` to run is `rsync` client.

`rpki-prover` is a daemon that runs periodic re-validation of all TAs in the RPKI hierachy. The results of these runs are exposes in UI, JSON API and Prometheus metrics. Also the `--with-rtr` option enables RTR server pushing VRP updates to RTR clients.

There is no config file and all the configuration is provided with CLI (most of the defaults are pretty reasonable, so normally you don't need to adjust a lot of parameters).

There is an initialise step necessary to start after downloading or building the executable: you need to run something like `rpki-prover.exe --initialise --rpki-root-directory /var/where-you-want-data-to-be` to create the necessary FS layout in `/var/where-you-want-data-to-be`. It will download the TAL files to `/var/where-you-want-data-to-be/tals` as well. The awkward part related to ARIN TAL license agreement is pretty much a rip off from the Routinator implementation as the most convenient for the user.

## Static Linux binary

Every [release](https://github.com/lolepezy/rpki-prover/releases) includes statically linked Linux x64 executable, just download and run it. 

## Docker image

It is possible to run rpki-prover as `docker run lolepezy/rpki-prover:latest`. The image is available on Docker Hub and it's about 80mb in size.

It is also possible to build your own image using `docker build . --file Dockerfile.prover --tag rpki-prover`.

Since `rpki-prover` needs to have some persistent directory to use for TALs, caches, temporary files, etc. (the aforementioned `/var/where-you-want-data-to-be`), there needs to be a persistent volume configured for it, so typical sequence of commands could be something like this

```
docker volume create rpki-data
docker pull lolepezy/rpki-prover:latest
docker run --mount source=rpki-data,target=/rpki-data lolepezy/rpki-prover:latest --initialise --agree-with-arin-rpa
docker run -p 9999:9999 --mount source=rpki-data,target=/rpki-data lolepezy/rpki-prover:latest --cpu-count 4 --revalidation-interval 300
``` 
The important part here is `target=/rpki-data`, this directory is created by default inside of the docker container. Otherwise it can be adjusted as in
```
docker run -p 9999:9999 --mount source=rpki-data,target=/something-else lolepezy/rpki-prover:latest --rpki-root-directory /something-else
```

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


## HTTP API

There are several API endpoints. Note that there is not much of design invested into the API, so more adjustments will come based on the feedback and feature requests.

- `/api/vrps.csv`  - CSV-formatted list of VRPs
- `/api/vrps.json` - JSON-formatted list of VRPs
- `/api/validation-result` - JSON-formatted latest validation results similar to what can be seen in the UI
- `/api/app-metrics` - JSON-formatted latest metrics similar to what can be seen in the UI
- `/api/lmdb-stat` - JSON-formatted statistics for LMDB cache. Normally it is not of a huge interest to anyone, mostly used for development and testing.
- `/api/object` - JSON-formatted dump of the RPKI object. 
    - With the `hash` paramter the object will be picked by hash 
    - With the `uri` paramter the object will be picked by its URL 
    
    For example 
    - http://localhost:9999/api/object?uri=rsync://rpki.apnic.net/member_repository/A917D135/19712F8613DD11EB8FFBED74C4F9AE02/0u36vBm4dKB-OdauyKaLMLuB2lw.crl
    - or http://localhost:9999/api/object?hash=a92126d3a58a0f6593702f36713521fcd581e0a9e38a146f7f762c7d7d48ed0a

## Prometheus metrics 

Prometheus metrics are accessible via the standard `/metrics` path.

## Resource consumption

Cold start, i.e. the first start without cache takes at least 2 minutes and consumes around 3 minutes of CPU time. This time can be slightly reduced by setting higher `--cpu-count` value in case multiple CPUs are available. While CPU-intensive tasks scale pretty well (speed-up is sublinear up to 8-10 CPU cores), the total warm up time is moslty limited by the download time of the slowest of RPKI repositories and cannot be reduced drastically. 

After initial warmup, it's not a very CPU-bound application. With default settings RPKI Prover consumes about 1 hour of CPU time every 18 hours on a typical modern CPU, creating load average of 5-10%. Smaller revalidation interval will increase the load.

The amount of memory needed for a smooth run for the current state of the repositories (6 trust anchors, including [AS0 TA](https://www.apnic.net/community/security/resource-certification/tal-archive/) of APNIC with about 330K of VRPs in total) is somewhere around 1.5GB. Adding or removing TAs can increase or reduce this amount. What can be confusing about memory usage is the figures given by `top/htop`.

An example of a server, running for a few days:
```
VIRT  RES    SHR
1.0T  5383M  3843M
```
Here `SHR` is largely dominated by the LMDB cache and other mmap-ed files (temporary files used to download RRDP repositories, etc.). That means that actual heap of the process is about `5383-3843=1540M`.

Note that memory consumption is mostly determined by how big the biggest objects are and not that much by how many there are objects in total, so the growth of repositories is not such a big issue for rpki-prover. It it recommended to have 3GB of RAM available on the machine mostly to reduce the IOPS related to reading objects from the LMDB cache. Since every validation typically goes through 160K of objects (at the moment of writing), each of them being 3Kb in size on average, it would be benificial to have at least few hundred of megabytes in FS page cache.

Disk space usage depends on the `--cache-lifetime-hours` parameter. The default is 72 hours and it results in a cache size about 2Gb. 72 hours is a little bit on a big side, so lower values would reduce the amount of data stored. However, LMDB is not very good in reusing the free space in its file, so physical size of the `cache` directory can be 2 or more times bigger than the total size of data in it. There is a compaction procedure that kicks in when the LMDB file size is 2 or more times bigger than the total size of all data. So overall, in the worst case scenario, it would need approximately 1GB of disk space for every 10 hours of `--cache-lifetime-hours`.

## Known issues

 - From time to time a message 'rpki-prover: Thread killed by timeout manager' may be printed to `stderr`. It's the result of a bug in the HTTP server used for API and UI and is harmless. It will be fixed one way or the other in future versions.
 - As mentioned before, total RSS of the process can go up to several gigabytes even though most of it mapped to LMDB cache and not in RAM. It may, however, be that `rpki-prover` is killed by OOM and some configuration adjustments would be needed to prevent it.

 ## Why Haskell?

- Relatively small code-base. Currently the size of it is around 10KLOC, including a lot of functionality implemented from scratch, such as CMS-parsing.
- Fast prototyping and smooth refactoring.
- Ease of introducing changes and very short time-to-market.
- Reasonable performance while the language is very high-level (GC, immutable data, powerful type system).
- Original motivation was "because it's cool", everything else came later.
