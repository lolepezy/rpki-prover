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
