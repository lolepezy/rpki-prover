/*
 * Cap glibc's per-thread malloc arenas.
 *
 * SQLite is hit from many OS threads (the read connection pool, plus every
 * in-flight FFI call), and by default glibc hands each contending thread its
 * own 64MB arena that is never returned to the OS. Left alone, RES grows into
 * the gigabytes while the actual GHC heap stays flat.
 *
 * This has to happen in an ELF constructor rather than in Haskell `main`:
 * glibc computes its arena limit lazily, the first time a thread asks for a
 * new arena, and caches it in a static (`narenas_limit` in `arena_get2`).
 * Once that has happened -- which it has by the time `main` runs, since the
 * RTS starts its capability threads first -- `mallopt` is silently ignored.
 * A constructor runs while the process is still single-threaded, before the
 * RTS is initialised, so the limit is in place before any arena is created.
 *
 * MALLOC_ARENA_MAX in the environment takes precedence, so this can still be
 * tuned per deployment without a rebuild.
 *
 * NB: stdlib.h has to come first -- __GLIBC__ is only defined once a libc
 * header has pulled in features.h, so guarding on it before any #include
 * silently compiles the whole file away.
 */

#include <stdlib.h>

#if defined(__linux__) && defined(__GLIBC__)

#include <malloc.h>

__attribute__((constructor))
static void rpki_prover_tune_malloc(void)
{
    if (getenv("MALLOC_ARENA_MAX") == NULL) {
        mallopt(M_ARENA_MAX, 2);
    }

    /*
     * Pin the mmap threshold at glibc's own default instead of letting it
     * float upwards.
     *
     * By default a large allocation is served by mmap, but when such a block
     * is freed glibc raises the threshold (up to 32MB) on the theory that the
     * program will want that size again. After that, allocations of that size
     * come from the arena and stay there when freed. Reading payload BLOBs out
     * of SQLite is exactly that pattern -- a few multi-megabyte buffers per
     * validation round -- and it left the allocator sitting on ~76mb of freed
     * space that only malloc_trim could hand back.
     *
     * Setting this through mallopt also disables the dynamic adjustment, so
     * large blocks keep going through mmap and are returned to the OS on free.
     * Measured over a run: allocator footprint 84mb -> 12mb, freed-but-held
     * 76mb -> 4mb, peak RSS 599mb -> 530mb, with no change to validation or
     * payload re-read times.
     */
    if (getenv("MALLOC_MMAP_THRESHOLD_") == NULL) {
        mallopt(M_MMAP_THRESHOLD, 128 * 1024);
    }
}

#endif
