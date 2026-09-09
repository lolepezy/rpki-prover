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
     * validation round -- so the threshold floats up to its maximum and the
     * allocator ends up sitting on the high-water mark of that churn.
     *
     * Setting this through mallopt also disables the dynamic adjustment, so
     * large blocks keep going through mmap and are returned to the OS on free.
     * This one line is what keeps the C allocator's freed-but-held space flat:
     * measured over runs of the same instance on the same cache, median 73mb
     * to 202mb without it against 4mb with it, never exceeding 5.3mb over 5.75
     * hours. Validation and payload re-read times are unaffected either way.
     *
     * Do not be tempted to reach for malloc_trim() instead. glibc's own
     * trimming only shrinks the top of the heap (mallinfo2's keepcost, which
     * was under 1mb here while the free lists held ~150mb), so it can return
     * almost nothing, and an explicit malloc_trim() leaves the free lists in
     * place in mallinfo2's accounting -- so any trigger reading fordblks
     * latches on and never clears. That is what the periodic trim this
     * codebase used to carry ran into; with the threshold pinned it never had
     * anything to do, and it is gone.
     */
    if (getenv("MALLOC_MMAP_THRESHOLD_") == NULL) {
        mallopt(M_MMAP_THRESHOLD, 128 * 1024);
    }
}

#endif
