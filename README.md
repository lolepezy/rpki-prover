# rpki-prover

Implementation of the RPKI relying party software.

Currently implemnted features are

- Downloading from both rsync and RRDP repositories
- X509 validation and validation of EE certificates 
- Validation of resource sets, including support for RFC8360 "validation reconsidered"
- Output of VRPs in CSV and JSON formats
- Output of found problems

Future work
- Support for RTR protocol
- SLURM support
- Fancy UI

