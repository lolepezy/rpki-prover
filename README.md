# rpki-prover

Implementation of the RPKI relying party software with the focus on a reasonable compromise between resource utilisation and ease of introducing changes.

At the moment it is only the daemon written in Haskell and there's some work planned for the UI.

Some of the decisions about the architecture and general flow are documented in ./doc/*.md files. The doc/TODO.md file includes all the currently planned issue fixes and improvements.

Currently implemented features are

- Fetching from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- Output of VRPs in CSV and JSON formats
- Output of found problems
- Cache cleanup, scheduled revalidation
- Support for RTR protocol

Current and future work
- Adjustment to the latest RFC 6486-bis: rrdp -> rsync fallback, "failed fetch" concept
- Static binaries (at least for linux), packaging and a Docker image
- SLURM support
- Fancy UI

The only option to use it at the moment is to build from sources:

The instruction below is for linux, but it can work equally for \*BSD or Mac (Windows support is not planned or tested).
    
   - Install libraries for: `lmdb`, `lzma`
      - On linux using apt-get, that will be : `sudo apt-get install liblmdb-dev liblzma-dev`
      - On MacOS using brew, that will be: `brew install lmdb xz` 
      - It should be trivial to find the corresponding commands for other UNIX-like OSes or package managers
   - Install `stack` as described here `https://docs.haskellstack.org/en/stable/install_and_upgrade/`
   - Clone https://github.com/lolepezy/rpki-prover/
   - Run `stack install rpki-prover:rpki-prover` inside of the rpki-prover. It should take quite some time (30-50 minutes as it has to build all the required libraries)
   - Run `mkdirs.sh` script. It will create some directory structure inside of ~/.rpki and download TAL files from `https://github.com/NLnetLabs/routinator/tree/master/tals` (kudos, guys!)
   - Run `rpki-prover` from the `~/.local/bin`

Running Run `rpki-prover --help` gives some help on the CLI options.

Normally it prints quite a lot of logs about what it's doing to the stdout. After it prints "Validated all TAs, took ..." (it should take 2-4 minutes depending on how fast the CPU and network are) VRPs can be fetched by executing `curl -s http://localhost:9999/api/vrps.csv` (or `curl -s http://localhost:9999/api/vrps.json`).


 
