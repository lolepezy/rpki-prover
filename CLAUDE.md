# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

RPKI Prover is RPKI relying-party software: a daemon that periodically validates the RPKI certificate hierarchy (fetching from rsync and RRDP repositories), computes VRPs, and serves them via HTTP API, RTR (RFC 8210), and a UI. It's a Haskell project (~20 KLOC), built with `cabal`, using `hpack` (`package.yaml` is the source of truth; `rpki-prover.cabal` is generated from it — edit `package.yaml`, not the `.cabal` file).

## Build & run

GHC/cabal versions are pinned in `.tool-versions` (currently GHC 9.10.3 / cabal 3.16.1.0), managed via [ghcup](https://www.haskell.org/ghcup/) on Linux/macOS. See `DEVELOPER.md` for OS package prerequisites (`lmdb`, `lzma`, `expat`, `gmp`, `rsync`) and the OpenBSD-specific setup (ghcup doesn't support OpenBSD; there's a separate `build-openbsd.sh` / `Vagrantfile` path).

- **`./build-local.sh`** — full build via `cabal install`, installs `rpki-prover` to `~/.cabal/bin` (or `~/.local/bin`). Takes 30-50 minutes from a cold cache.
- **`./run-tests.sh`** — runs the test suite (`cabal test test:rpki-prover-test`).
  - To run a single test/group, use tasty's pattern matching directly: `cabal run rpki-prover-test -- --pattern "<substring>"`.
- **`./build-static.sh`** — builds a statically-linked Linux binary via a musl-based Docker builder (`Dockerfile.static-builder`). This is what release binaries are built from.
- **`./build-docker.sh`** / **`./docker-release.sh X.Y.Z`** — build/push the `lolepezy/rpki-prover` Docker image (`Dockerfile.prover`).
- **`./build-profile.sh`** — builds the `profiler` executable (`perf/Profile.hs`).
- **`./deploy.sh <root-dir> [--reset]`** — builds and (re)launches a locally-running instance for the current branch, with a deterministic port derived from the branch path; used for manual dev testing against real repositories.
- **`ghcid`** works as-is for fast feedback.

**Important gotcha**: `src/RPKI/Meta/GitVersionInfo.hs` and the source hash in `src/RPKI/Meta/UniqueId.hs` are generated (by `generate-modules.sh`, sourced automatically by all the `build-*.sh` scripts) and are required for compilation. If you invoke `cabal build`/`cabal repl`/`cabal test` directly instead of through one of the wrapper scripts, run `./generate-modules.sh` first.

### Bumping the DB version

`currentDatabaseVersion` in `Database.hs` must be bumped whenever LMDB (de)serialization could break — i.e. whenever data types in `Domain.hs` change, or when a library whose types get persisted changes version. A mismatch just makes the prover wipe its cache and start fresh on upgrade, so it's cheap to over-bump.

## Architecture

### Execution modes (`app/Main.hs`)

A single binary, dispatched by CLI flags parsed with `optparse-applicative` (`CLIOptions` in `Main.hs`):
- Normal daemon mode: periodic validation loop (`RPKI.Workflow`), HTTP API, optional RTR server.
- `--once`: one-off validation run instead of a persistent daemon.
- `--verify-signature`: RSC (RPKI Signed Checklist) verification against an already-populated cache, not a daemon run.
- `--worker`: internal — this is how the binary re-execs itself as an isolated subprocess (see below); not meant to be invoked directly by users.

### Process-isolated workers (`RPKI.Worker`)

CPU/memory/time-bounded work (rsync fetches, RRDP fetches, per-TA validation, etc.) runs in child processes rather than threads, spawned by re-executing the same binary with `--worker`. A worker reads a serialized `WorkerInput` from stdin, writes a serialized `WorkerResult` to stdout, and streams logs over stderr; `runWorker` in `RPKI.Worker` handles process lifecycle, timeouts, and CPU limits. This is a deliberate design choice for resilience: a runaway or crashing fetch/validation for one repository can't take down the whole daemon or corrupt shared state, and can be killed on a timeout/memory limit. `RPKI.Workflow` orchestrates cleanup of orphaned worker processes.

### Validation pipeline

- **Parsing** (`RPKI.Parse.*`): hand-rolled parsers for RPKI object types (certs, CRLs, MFTs, ROAs, ASPAs, GBRs, RSC, SPL) over DER-encoded CMS signed objects — no external ASN.1/CMS library dependency for this part.
- **Fetching** (`RPKI.Fetch`, `RPKI.Rsync`, `RPKI.RRDP.RrdpFetch`, `RPKI.Repository`): each publication point is RRDP-first with rsync fallback per RFC guidance (`PublicationPointAccess` in `Repository.hs` models this). Rsync shells out to an external `rsync` client (configurable path); RRDP does incremental delta-vs-snapshot fetching.
- **Validation** (`RPKI.Validation.TopDown`, `.BottomUp`, `.ResourceValidation`, `.ObjectValidation`): top-down walk of the certificate tree from each TA, RFC 8360 "validation reconsidered" resource handling, BGPSec, ASPA, RSC, SPL, prefix-list support.
- **SLURM** (`RPKI.SLURM.*`): RFC 8416 local filtering/exceptions applied on top of validated VRPs.

### Storage (`RPKI.Store.*`)

LMDB-backed (via the `lmdb-high-level` package, patched fork pinned in `cabal.project`), memory-mapped for large-object efficiency. `Store.Base.*` is a generic typed key-value/multimap layer over raw LMDB; `Store.Database` builds the actual typed tables (objects, repositories, VRPs, metrics, etc.) on top of it; `Store.MakeLmdb` wires up the environment/settings. See "Bumping the DB version" above — this is the layer that versioning protects.

### Interfaces

- **HTTP** (`RPKI.Http.*`): `servant`-based API (`Api.hs`), UI (`UI.hs`), served via `warp`. Swagger UI at `/swagger-ui`, Prometheus metrics at `/metrics` (`RPKI.Metrics.Prometheus`), VRPs and other objects in CSV/JSON.
- **RTR** (`RPKI.RTR.*`): RFC 8210 RTR server (protocol versions 0 and 1), enabled via `--with-rtr`.
- **Config/state**: `RPKI.Config` holds the fully-resolved CLI-derived configuration; `RPKI.AppState` holds mutable runtime state (STM); `RPKI.AppContext` bundles logger/config/state/DB handle and is threaded through most of the codebase; `RPKI.AppMonad` defines the validation monad stack (error/warning accumulation via `ValidatorT`, not plain exceptions, for most of the validation code).

### Testing

`test/Spec.hs` is the tasty entry point aggregating per-module specs under `test/src/RPKI/...` (mirrors `src/RPKI/...` structure). `test/data/` holds fixture RPKI objects (certs, MFTs, ROAs, ASPAs, GBRs, SLURM JSON, etc.) used by parser/validation tests.
