# SQLite Migration Plan

Replace LMDB with SQLite as the storage backend.

## Motivation

From the design discussions (see shared Claude / ChatGPT chats):

1. **SQL joins & relational integrity** — a large fraction of `Database.hs` is
   hand-rolled secondary indexing (`certBySKI`, `mftsForKI`, `hashToKey`,
   `objectMetas`, URL maps). Moving to SQL replaces every manual `SMap`/`SMultiMap`
   index with a `CREATE INDEX` declaration, and makes cross-store queries trivial
   joins instead of multi-step Haskell code.
2. **Eliminate compaction** — LMDB requires a full copy-rewrite cycle to reclaim
   fragmented space (`compactStorageWithTmpDir` / `runCopyWorker`). SQLite reuses
   freed pages automatically; WAL checkpointing replaces this.
3. **Built-in write-timeout** — the manual `withTransactionWrapper` / `monitorDbState`
   / `TxTimeout` machinery can be replaced with `PRAGMA busy_timeout`.
4. **Remove stale-reader cleanup** — `cleanupReaders` / `cleanUpStaleTx` exists
   solely because LMDB reader slots can leak. SQLite POSIX advisory locks are
   released unconditionally by the OS on process exit.
5. **Multi-process** — SQLite in WAL mode is designed for exactly this use pattern
   (main process + validation worker + fetcher workers, each with their own
   connection). No new constraints vs LMDB.

---

## Architecture Change: Direct SQL, No Storage Typeclass

The `Storage`/`SMap`/`SMultiMap` abstraction is **removed entirely**. All database
operations become direct `sqlite-simple` SQL queries written in `Database.hs`.

Key consequences:

- `DB s` loses the type parameter and becomes just `DB`.
- `Tx s 'RO` / `Tx s 'RW` become `Tx 'RO` / `Tx 'RW` — a thin newtype around
  `Connection` with a phantom mode parameter. The RO/RW division of call sites in
  `Database.hs` and `Validation/` is preserved as-is.
- The typed relational schema replaces the generic blob key-value tables.
- `Database.hs` public function **signatures** stay the same so callers in
  `Validation/` need no changes; only the internals change.
- `certBySKI` is fixed (returns `[ObjectKey]`) as part of this migration, not a
  separate step.
- `object_meta` fields (`WorldVersion`, `RpkiObjectType`) are merged directly into
  the `objects` table — no join needed for the common case.

---

## Overview of Affected Files

### Files to create

| New file | Purpose |
|---|---|
| `src/RPKI/Store/SQLite.hs` | Connection pool, transaction helpers, schema init |
| `src/RPKI/Store/AppSqliteStorage.hs` | Lifecycle: setup, checkpoint, stats, reopen |

### Files to rewrite entirely

| File | Nature of change |
|---|---|
| `src/RPKI/Store/Database.hs` | Same public API; all internals become direct SQL queries |
| `src/RPKI/Store/AppStorage.hs` | Remove `MaintainableStorage` typeclass or simplify to plain functions |

### Files to delete

- `src/RPKI/Store/Base/LMDB.hs`
- `src/RPKI/Store/Base/Storage.hs` — `Storage`/`WithTx` typeclass gone
- `src/RPKI/Store/Base/Map.hs`
- `src/RPKI/Store/Base/MultiMap.hs`
- `src/RPKI/Store/Base/SafeMap.hs` — no key-length limit in SQLite
- `src/RPKI/Store/MakeLmdb.hs`
- `src/RPKI/Store/AppLmdbStorage.hs`
- `src/RPKI/Store/Sequence.hs` — `object_key` is now the SQLite ROWID; `nextValue`/`SequenceMap` go away

### Files to modify

| File | Change |
|---|---|
| `rpki-prover.cabal` | Replace `lmdb-high-level` with `sqlite-simple` + `resource-pool` |
| `RPKI.Config` | Remove `lmdbSizeMb`, add `sqliteBusyTimeoutMs` / `sqlitePoolSize` |
| `app/Main.hs` / wiring | Swap `setupLmdbCache` → `setupSqliteCache` |

### Files with no changes needed

All of `src/RPKI/Validation/` and any code that only calls the public functions of
`Database.hs` (signatures are preserved).

---

## Phase 1 — Connection & Transaction Layer

**File: `src/RPKI/Store/SQLite.hs`**

### DB type

```haskell
data DB = DB
    { readPool  :: Pool Connection  -- shared read connections (8 recommended; benchmark)
    , writeConn :: MVar Connection  -- single dedicated write connection
    }
```

A dedicated MVar-serialised write connection is simpler than `BEGIN IMMEDIATE`
contention across a shared pool. Read connections come from the pool.

### Tx newtype

```haskell
data TxMode = RO | RW

-- Phantom wrapper; keeps the RO/RW call-site discipline from the current code.
newtype Tx (m :: TxMode) = Tx { unTx :: Connection }
```

### Transaction helpers

```haskell
withReadTx :: MonadIO m => DB -> (Tx 'RO -> IO a) -> m a
withReadTx DB{readPool} f = liftIO $ withResource readPool $ \conn -> do
    execute_ conn "BEGIN"
    f (Tx conn) `finally` execute_ conn "COMMIT"

withWriteTx :: MonadIO m => DB -> (Tx 'RW -> IO a) -> m a
withWriteTx DB{writeConn} f = liftIO $ withMVar writeConn $ \conn -> do
    execute_ conn "BEGIN IMMEDIATE"
    result <- try (f (Tx conn))
    case result of
        Left  (e :: SomeException) -> execute_ conn "ROLLBACK" >> throwIO e
        Right v                    -> execute_ conn "COMMIT"   >> pure v
```

These replace `roTx`/`rwTx`, `roAppTx`/`rwAppTx`, `roTxT`/`rwTxT`. Call sites in
`Database.hs` and `Validation/` continue to pass `tx :: Tx 'RO` or `tx :: Tx 'RW`
as the first argument — the only change is that `unTx tx` is used internally to get
the `Connection` for `sqlite-simple` calls. Functions restricted to writes continue
to require `Tx 'RW`.

Most `roTx` calls in `TopDown` are already short-lived (single object or object +
locations), so WAL truncation is not impacted.

### Connection initialisation (applied to every connection at creation)

```haskell
initConn :: Int -> FilePath -> IO Connection
initConn busyTimeoutMs path = do
    conn <- open path
    mapM_ (execute_ conn)
        [ "PRAGMA journal_mode = WAL"
        , "PRAGMA foreign_keys = ON"
        , "PRAGMA busy_timeout = " <> fromString (show busyTimeoutMs)
        , "PRAGMA synchronous = NORMAL"
        , "PRAGMA optimize = 0x10002"
        ]
    pure conn
```

`busy_timeout` replaces the manual `withTransactionWrapper` / `TxTimeout` mechanism.
`PRAGMA synchronous = NORMAL` is safe with WAL (no data loss on OS crash).

---

## Phase 2 — Schema

All `CREATE TABLE IF NOT EXISTS` statements live in `SQLite.hs` (or a dedicated
`initSchema` function called at startup). `eraseCache` (called on version
incompatibility) runs `DROP TABLE IF EXISTS` for every table name, then re-runs
`initSchema`. No file deletion needed.

```sql
-- Primary object store.
-- ObjectMeta fields (world_version, object_type) are merged here — no join needed.
-- data is nullable: saveOriginal stores raw-only rows where data IS NULL and original IS NOT NULL.
-- saveObject stores fully-parsed rows where data IS NOT NULL.
CREATE TABLE IF NOT EXISTS objects (
    object_key    INTEGER PRIMARY KEY,
    hash          BLOB    NOT NULL UNIQUE,
    type          TEXT    NOT NULL,       -- 'CER','MFT','ROA','CRL','ASPA','GBR','SPL','RSC','ORIGINAL'
    data          BLOB,                  -- Compressed (StorableObject RpkiObject); NULL for raw-only rows
    original      BLOB,                  -- Verbatim ObjectOriginal; NOT NULL for raw-only rows
    world_version INTEGER NOT NULL,
    object_type   TEXT    NOT NULL,
    CHECK (data IS NOT NULL OR original IS NOT NULL)
);

-- URL catalogue (replaces the uriToUriKey / uriKeyToUri pair).
-- url_key is ROWID alias; insert with INSERT OR IGNORE, read back with last_insert_rowid().
CREATE TABLE IF NOT EXISTS urls (
    url_key INTEGER PRIMARY KEY,
    url     TEXT    NOT NULL UNIQUE
);

-- Object ↔ URL mapping (replaces urlKeyToObjectKey + objectKeyToUrlKeys).
CREATE TABLE IF NOT EXISTS object_urls (
    object_key INTEGER NOT NULL REFERENCES objects(object_key) ON DELETE CASCADE,
    url_key    INTEGER NOT NULL REFERENCES urls(url_key) ON DELETE CASCADE,
    PRIMARY KEY (object_key, url_key)
);
CREATE INDEX IF NOT EXISTS idx_object_urls_url ON object_urls(url_key);

-- Certificate index (replaces certBySKI — non-unique index, multiple per SKI allowed).
CREATE TABLE IF NOT EXISTS certificates (
    object_key INTEGER NOT NULL PRIMARY KEY REFERENCES objects(object_key) ON DELETE CASCADE,
    ski        BLOB    NOT NULL,
    aki        BLOB
);
CREATE INDEX IF NOT EXISTS idx_cert_ski ON certificates(ski);

-- Manifest metadata (replaces mftsForKI).
CREATE TABLE IF NOT EXISTS manifest_meta (
    object_key      INTEGER NOT NULL PRIMARY KEY REFERENCES objects(object_key) ON DELETE CASCADE,
    aki             BLOB    NOT NULL,
    manifest_number INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_mft_aki ON manifest_meta(aki);

-- Manifest shortcuts (replaces mftShortcuts two-map pair).
CREATE TABLE IF NOT EXISTS mft_shortcut_meta (
    aki  BLOB NOT NULL PRIMARY KEY,
    data BLOB NOT NULL
);
CREATE TABLE IF NOT EXISTS mft_shortcut_children (
    aki  BLOB NOT NULL PRIMARY KEY,
    data BLOB NOT NULL
);

-- Trust anchors (replaces SafeMap "trust-anchors").
CREATE TABLE IF NOT EXISTS trust_anchors (
    ta_name TEXT NOT NULL PRIMARY KEY,
    data    BLOB NOT NULL
);

-- Repository store (replaces the four SafeMap fields of RepositoryStore).
-- 'kind' discriminates: 'rrdp-pp','rsync-pp','rrdp-vstate','rsync-vstate'
CREATE TABLE IF NOT EXISTS repositories (
    key  TEXT NOT NULL,
    kind TEXT NOT NULL,
    data BLOB NOT NULL,
    PRIMARY KEY (key, kind)
);

-- Large blob stores keyed by an integer (validations, metrics, payload sets).
-- These are written/read as single compressed blobs per TA version.
CREATE TABLE IF NOT EXISTS validations (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS metrics     (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS roas        (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS spls        (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS aspas       (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS gbrs        (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS bgps        (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS slurm       (key INTEGER PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS versions    (key BLOB NOT NULL PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS jobs        (key TEXT NOT NULL PRIMARY KEY, value BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS metadata    (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL);

-- Per-TA validated-by-version map (single compressed blob per key).
CREATE TABLE IF NOT EXISTS validated_by_version (
    key   TEXT NOT NULL PRIMARY KEY,
    value BLOB NOT NULL
);
```

### Key schema decisions

- **`object_meta` merged into `objects`** — `ObjectMeta wv (getRpkiObjectType object)`
  becomes `world_version` and `object_type` columns. Eliminates a join on every
  object lookup.
- **`hashToKey` → `UNIQUE` on `objects.hash`** — `getKeyByHash` becomes
  `SELECT object_key FROM objects WHERE hash = ?`.
- **`certBySKI` → `certificates` with non-unique index on `ski`** — `getBySKI`
  returns `[ObjectKey]`; `BottomUp.hs` tries each candidate and takes the first
  whose signature verifies. Fixes the correctness bug from the Claude discussion.
- **`deleteObjectByKey` simplifies** — `DELETE FROM objects WHERE object_key = ?`
  with `ON DELETE CASCADE` cleans up `certificates`, `manifest_meta`,
  `object_urls` automatically.
- **URL tables** — four-way structure (`uriToUriKey`/`uriKeyToUri`/
  `urlKeyToObjectKey`/`objectKeyToUrlKeys`) collapses to two tables with standard
  FK + index.
- **`originals` (raw-only objects)** — `saveOriginal` creates a new `ObjectKey` and
  stores the raw bytes without a parsed `RpkiObject`. In SQLite this maps to a row in
  `objects` with `data = NULL` and `original = <bytes>`. The `hash → object_key`
  relationship is preserved via the existing `UNIQUE` on `objects.hash`; no separate
  table is needed.
- **`SafeMap` overflow path** — deleted; SQLite has no key-size limit.
- **`eraseCache`** — `DROP TABLE IF EXISTS <t>` per table, then `initSchema`.

---

## Phase 3 — Rewrite Database.hs

The public function signatures stay the same so callers in `Validation/` need no
changes. The `DB s` type parameter is dropped (`DB` with no parameter). `Tx s m`
becomes `Connection`. `roAppTx`/`rwAppTx` delegate to `withReadTx`/`withWriteTx`.

### Function mapping (representative examples)

| Current | SQLite implementation |
|---|---|
| `saveObject tx db so wv` | `INSERT OR IGNORE INTO objects (hash, ...) VALUES (?,...) RETURNING object_key`; on conflict (hash already present) fall back to `SELECT object_key FROM objects WHERE hash = ?`; no manual sequence — ROWID is the key |
| `deleteObjectByKey tx db k` | `DELETE FROM objects WHERE object_key = ?` (CASCADE handles indexes) |
| `getObjectByKey tx db k` | `SELECT data FROM objects WHERE object_key = ?` |
| `getKeyByHash tx db h` | `SELECT object_key FROM objects WHERE hash = ?` |
| `getMftsForAKI tx db aki` | `SELECT m.object_key, m.manifest_number, o.data FROM manifest_meta m JOIN objects o USING(object_key) WHERE m.aki = ? ORDER BY m.manifest_number DESC` |
| `getBySKI tx db ski` | `SELECT object_key FROM certificates WHERE ski = ?` — returns `[ObjectKey]` |
| `getLocatedByKey tx db k` | `SELECT o.data, u.url FROM objects o JOIN object_urls ou USING(object_key) JOIN urls u USING(url_key) WHERE o.object_key = ?` |
| `getObjectsStats tx db` | `SELECT type, COUNT(*), SUM(LENGTH(data)) FROM objects GROUP BY type` |
| `saveOriginal tx db orig hash meta` | `INSERT OR IGNORE INTO objects (hash, type, data, original, world_version, object_type) VALUES (?, 'ORIGINAL', NULL, ?, ?, ?) RETURNING object_key` |
| `getOriginalBlob tx db key` | `SELECT original FROM objects WHERE object_key = ?` |

### `certBySKI` correctness fix

`getBySKI` returns `[ObjectKey]` instead of `Maybe ObjectKey`. Update `BottomUp.hs`
`findPathToRoot` to iterate candidates and take the first whose signature verifies —
mirroring the resilient fallback pattern `TopDown.hs` already uses for manifests.

### Transaction wiring

```haskell
-- In Database.hs, wrapping withReadTx/withWriteTx from SQLite.hs:
roAppTx :: MonadIO m => DB -> (Tx 'RO -> IO a) -> m a
roAppTx = withReadTx

rwAppTx :: MonadIO m => DB -> (Tx 'RW -> IO a) -> m a
rwAppTx = withWriteTx
```

SQL queries inside `Database.hs` functions extract the connection with `unTx tx`.
The type signature enforces that read-only functions cannot accidentally receive a
write transaction and vice versa, preserving the current invariant.

---

## Phase 4 — AppSqliteStorage.hs & Config

**`src/RPKI/Store/AppSqliteStorage.hs`**

| LMDB function | SQLite equivalent |
|---|---|
| `setupLmdbCache` | `setupSqliteCache` — ensure `<cacheDir>/rpki-cache.sqlite` exists, open pool, run `initSchema` |
| `setupWorkerLmdbCache` | `setupWorkerSqliteCache` — open the same `.sqlite` file read-pool only |
| `compactStorageWithTmpDir` | `sqliteCheckpoint`: `PRAGMA wal_checkpoint(TRUNCATE)` on the write connection |
| `reopenLmdbStorage` | Drain and recreate the pool |
| `cleanupReaders` | **Deleted** — not needed |
| `lmdbGetStats` | `SELECT SUM(pgsize) FROM dbstat` or `PRAGMA page_count * page_size` |
| `cacheFsSize` | File sizes of `.sqlite` + `.sqlite-wal` + `.sqlite-shm` |

Cache layout: `<cacheDir>/rpki-cache.sqlite`. No symlink, no `lmdb.N` directories,
no `generateLmdbDir`, no `removePossibleOtherLMDBCaches`.

**`RPKI.Config`** — replace:

```haskell
-- remove
lmdbSizeMb :: Size

-- add
sqliteBusyTimeoutMs :: Int   -- default 10_000 (ms); replaces rwTransactionTimeout
sqlitePoolSize      :: Int   -- default 8
```

---

## Phase 5 — Delete LMDB Code

Once Phases 1–4 are working and all tests pass, delete:

- `src/RPKI/Store/Base/LMDB.hs`
- `src/RPKI/Store/Base/Storage.hs`
- `src/RPKI/Store/Base/Map.hs`
- `src/RPKI/Store/Base/MultiMap.hs`
- `src/RPKI/Store/Base/SafeMap.hs`
- `src/RPKI/Store/MakeLmdb.hs`
- `src/RPKI/Store/AppLmdbStorage.hs`
- `src/RPKI/Store/Sequence.hs`

And from `Workflow.hs`:

- `LmdbCompactTask` block (replace with a simple periodic `PRAGMA wal_checkpoint`
  call through `runMaintenance`)
- `cleanUpStaleTx` scheduling and call site

---

## Phase 6 — Testing

1. **Port `DatabaseSpec`** — `withDB`/`makeLmdbStuff`/`releaseLmdb` become
   SQLite equivalents using a temp file. All existing per-operation tests
   (`shouldRollbackAppTx`, `shouldPreserveStateInAppTx`, `shouldKeepKeyOrdering`)
   should pass unchanged.

2. **`shouldKeepKeyOrdering`** — verify `LexOrdKey64` byte encoding produces the
   same ordering under SQLite `BLOB` comparison (expected to hold; must be
   explicitly tested).

3. **`getBySKI` multimap** — new test: insert two certs with different object keys
   but the same SKI, assert both are returned by `getBySKI`.

4. **Multi-process write concurrency** — two OS processes against the same
   `.sqlite` file, concurrent writes, verify no data loss or deadlock beyond
   `busy_timeout`.

5. **Benchmark** — measure `TopDown` validation wallclock before and after the
   switch. LMDB gives zero-copy mmap reads; SQLite copies blobs. If regression is
   significant, profile before adding prepared-statement caching.

---

## Dependencies

```
# add
, sqlite-simple  >= 0.4
, resource-pool  >= 0.4

# remove
, lmdb-high-level
```

Also remove the vendored `lmdb-high-level` source from `dist-newstyle/src/`.
