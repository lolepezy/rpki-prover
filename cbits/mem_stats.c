/*
 * glibc allocator statistics, for RPKI.Metrics.Memory.
 *
 * The GHC RTS only ever tells us about the Haskell heap. Everything SQLite
 * allocates goes through the C allocator instead, so without this the
 * difference between "the Haskell heap" and "what the process actually costs"
 * is unattributable. mallinfo2 walks every arena, so this covers memory the
 * allocator holds on to after it has been freed, which is what makes RSS grow
 * without any corresponding growth in live data.
 *
 * NB: stdlib.h has to come first -- __GLIBC__ is only defined once a libc
 * header has pulled in features.h, so guarding on it before any #include
 * silently compiles the whole file away (see cbits/malloc_arenas.c).
 */

#include <stdlib.h>
#include <string.h>

#if defined(__linux__) && defined(__GLIBC__) && defined(__GLIBC_PREREQ)
#  if __GLIBC_PREREQ(2, 33)
#    define RPKI_HAVE_MALLINFO2 1
#  endif
#endif

#define RPKI_MALLOC_STATS_FIELDS 5

#ifdef RPKI_HAVE_MALLINFO2

#include <malloc.h>

/*
 * Fills `out` with, in order (all in bytes):
 *   0: arena     -- non-mmapped space allocated from the system
 *   1: hblkhd    -- space in mmapped regions
 *   2: uordblks  -- total allocated space (actually in use)
 *   3: fordblks  -- total free space (freed, but still held by the allocator)
 *   4: keepcost  -- top-most releasable block, what malloc_trim could return
 *
 * Returns 1 when the numbers are real, 0 when the platform can't provide them.
 */
int rpki_prover_malloc_stats(long long *out)
{
    struct mallinfo2 mi = mallinfo2();
    out[0] = (long long) mi.arena;
    out[1] = (long long) mi.hblkhd;
    out[2] = (long long) mi.uordblks;
    out[3] = (long long) mi.fordblks;
    out[4] = (long long) mi.keepcost;
    return 1;
}

/*
 * Hand back to the OS whatever the allocator is holding but not using.
 *
 * Reading large BLOBs out of SQLite (re-reading the payloads after every
 * validation, above all) allocates tens of megabytes at a time. Freeing them
 * returns the memory to the allocator's free lists, not to the OS, so the
 * arenas keep growing to the high-water mark of that churn. Returns 1 if any
 * memory was actually released.
 */
int rpki_prover_malloc_trim(void)
{
    return malloc_trim(0);
}

#else

/* musl has neither mallinfo2 nor malloc_trim -- and no arenas either, so
   there is nothing to report and nothing to give back. */
int rpki_prover_malloc_stats(long long *out)
{
    memset(out, 0, RPKI_MALLOC_STATS_FIELDS * sizeof *out);
    return 0;
}

int rpki_prover_malloc_trim(void)
{
    return 0;
}

#endif
