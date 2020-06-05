# rpki-prover

Implementation of the RPKI relying party software with 
the focus on a reasonable compromise between resource 
utilisation and ease of introducing changes. 

At the moment it is only the daemon written in Haskell
and there's some work planned on the UI.

Currently implemnted features are

- Fetching from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- Output of VRPs in CSV and JSON formats
- Output of found problems

Future work
- Support for RTR protocol
- SLURM support
- Packaging and a Docker image
- Fancy UI

Buildgin from sources:

The instruction below is for linux, but it can work equally for *BSD or Mac (Windows support is not planned or tested)
    
   1. Install these libraries: `sudo apt-get install liblmdb-dev liblzma-dev libexpat1-dev` (or your system's version of it)
   2. Install `stack` as described here `https://docs.haskellstack.org/en/stable/install_and_upgrade/`
   3. Clone https://github.com/lolepezy/rpki-prover/
   4. Do `stack install` inside of the rpki-prover
   5. Run `rpki-prover-exe` from the `~/.local/bin`

Normally it prints quite a lot of logs about what it's doing. After it prints "" VRPs can be fetched by executing
`curl -s http://localhost:9999/vrps.csv`.
 
