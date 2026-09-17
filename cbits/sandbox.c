/*
 * Landlock sandbox for worker processes.
 *
 * A worker is told what it may touch through environment variables set by
 * the parent process (see RPKI.Sandbox):
 *
 *   RPKI_PROVER_SANDBOX_RW  ':'-separated paths with full read/write access
 *   RPKI_PROVER_SANDBOX_RO  ':'-separated paths with read-only access
 *   RPKI_PROVER_SANDBOX_WRITES_ONLY  "1" to restrict only writing
 *
 * If RW or RO is set, everything else on the filesystem is denied, as are
 * TCP bind/connect, abstract unix sockets and signals to other processes
 * (as far as the kernel's Landlock version supports these).
 *
 * With WRITES_ONLY, only writing anywhere but the RW paths is denied (creating,
 * changing, removing and renaming files), and nothing else is restricted.
 * RO is meaningless then. It's for workers that run other programs, such as
 * the rsync client: they are restricted in the same way.
 *
 * This has to happen before the GHC runtime starts: Landlock only applies to
 * the thread that asks for it and to threads and processes created after that,
 * while a threaded RTS already runs a dozen OS threads by the time any Haskell
 * code can do anything. A constructor runs while the process is still
 * single-threaded.
 *
 * Nothing is written to stderr from here -- the parent expects only encoded
 * log messages there. The outcome is recorded instead and the worker's
 * Haskell code reports it and refuses to run if the sandbox was asked for
 * but couldn't be set up.
 */

#include <stdio.h>
#include <stdlib.h>

/* Keep in sync with RPKI.Sandbox */
#define SANDBOX_NOT_REQUESTED 0
#define SANDBOX_APPLIED       1
#define SANDBOX_UNSUPPORTED   2
#define SANDBOX_FAILED        3

static int  sandbox_status = SANDBOX_NOT_REQUESTED;
static int  sandbox_abi    = 0;
static char sandbox_message[1024];

int         rpki_prover_sandbox_status(void)  { return sandbox_status; }
int         rpki_prover_sandbox_abi(void)     { return sandbox_abi; }
const char *rpki_prover_sandbox_message(void) { return sandbox_message; }

#if defined(__linux__)

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#if defined(__GLIBC__)
#include <dlfcn.h>
#endif

/* Same numbers on every architecture, defined here for older headers */
#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#endif
#ifndef __NR_landlock_add_rule
#define __NR_landlock_add_rule 445
#endif
#ifndef __NR_landlock_restrict_self
#define __NR_landlock_restrict_self 446
#endif
#ifndef O_PATH
#define O_PATH 010000000
#endif

/* From <linux/landlock.h>, which may be missing or too old */
#define LL_CREATE_RULESET_VERSION (1U << 0)
#define LL_RULE_PATH_BENEATH      1

#define LL_FS_EXECUTE      (1ULL << 0)
#define LL_FS_WRITE_FILE   (1ULL << 1)
#define LL_FS_READ_FILE    (1ULL << 2)
#define LL_FS_READ_DIR     (1ULL << 3)
#define LL_FS_REFER        (1ULL << 13)
#define LL_FS_TRUNCATE     (1ULL << 14)
#define LL_FS_IOCTL_DEV    (1ULL << 15)
/* EXECUTE .. MAKE_SYM, i.e. everything Landlock ABI 1 knows about */
#define LL_FS_ABI_1        ((1ULL << 13) - 1)
/* Everything that changes the filesystem: WRITE_FILE and REMOVE_DIR .. MAKE_SYM */
#define LL_FS_WRITE_ABI_1  (LL_FS_ABI_1 & ~(LL_FS_EXECUTE | LL_FS_READ_FILE | LL_FS_READ_DIR))
/* Rights that make sense for a file rather than a directory */
#define LL_FS_FILE_RIGHTS  (LL_FS_EXECUTE | LL_FS_WRITE_FILE | LL_FS_READ_FILE | \
                            LL_FS_TRUNCATE | LL_FS_IOCTL_DEV)

#define LL_NET_BIND_TCP                (1ULL << 0)
#define LL_NET_CONNECT_TCP             (1ULL << 1)
#define LL_SCOPE_ABSTRACT_UNIX_SOCKET  (1ULL << 0)
#define LL_SCOPE_SIGNAL                (1ULL << 1)

struct ll_ruleset_attr {
    uint64_t handled_access_fs;
    uint64_t handled_access_net;
    uint64_t scoped;
};

struct ll_path_beneath_attr {
    uint64_t allowed_access;
    int32_t  parent_fd;
} __attribute__((packed));

#define ENV_RW "RPKI_PROVER_SANDBOX_RW"
#define ENV_RO "RPKI_PROVER_SANDBOX_RO"
#define ENV_WRITES_ONLY "RPKI_PROVER_SANDBOX_WRITES_ONLY"

static void fail(const char *what, const char *path)
{
    int e = errno;
    sandbox_status = SANDBOX_FAILED;
    if (path != NULL)
        snprintf(sandbox_message, sizeof sandbox_message, "%s %s: %s", what, path, strerror(e));
    else
        snprintf(sandbox_message, sizeof sandbox_message, "%s: %s", what, strerror(e));
}

/* Is this process a worker, i.e. was it started as `rpki-prover --worker ...`?
   Read from procfs since musl doesn't pass argv to constructors. */
static int is_worker_process(void)
{
    char buf[256];
    int fd = open("/proc/self/cmdline", O_RDONLY | O_CLOEXEC);
    if (fd < 0)
        /* Can't tell, the variables are an explicit request, so honour them */
        return 1;
    ssize_t n = read(fd, buf, sizeof buf - 1);
    close(fd);
    if (n <= 0)
        return 1;
    buf[n] = '\0';
    const char *arg1 = buf + strlen(buf) + 1;
    return arg1 < buf + n && strcmp(arg1, "--worker") == 0;
}

/* Load what the C library would otherwise load from disk later, when the
   sandbox no longer lets it. */
static void preload_libraries(void)
{
#if defined(__GLIBC__)
    /* glibc loads libgcc_s the first time a thread calls pthread_exit, to
       unwind its stack, and aborts the process if it can't. The GHC runtime
       calls pthread_exit when it retires spare OS threads. Once the library is
       loaded, glibc finds it by name without opening any files. Failing here
       is fine for static builds, they don't need it. */
    (void) dlopen("libgcc_s.so.1", RTLD_NOW | RTLD_NODELETE);
#endif
}

static uint64_t fs_rights_for_abi(int abi, int writes_only)
{
    if (writes_only) {
        uint64_t rights = LL_FS_WRITE_ABI_1;
        if (abi >= 2) rights |= LL_FS_REFER;
        if (abi >= 3) rights |= LL_FS_TRUNCATE;
        return rights;
    }
    uint64_t rights = LL_FS_ABI_1;
    if (abi >= 2) rights |= LL_FS_REFER;
    if (abi >= 3) rights |= LL_FS_TRUNCATE;
    if (abi >= 5) rights |= LL_FS_IOCTL_DEV;
    return rights;
}

static int add_rule(int ruleset, const char *path, uint64_t rights)
{
    int fd = open(path, O_PATH | O_CLOEXEC);
    if (fd < 0) {
        fail("cannot open", path);
        return 0;
    }

    struct stat st;
    if (fstat(fd, &st) != 0) {
        fail("cannot stat", path);
        close(fd);
        return 0;
    }
    if (!S_ISDIR(st.st_mode))
        rights &= LL_FS_FILE_RIGHTS;

    struct ll_path_beneath_attr rule = { .allowed_access = rights, .parent_fd = fd };
    long rc = syscall(__NR_landlock_add_rule, ruleset, LL_RULE_PATH_BENEATH, &rule, 0);
    close(fd);
    if (rc != 0) {
        fail("landlock_add_rule", path);
        return 0;
    }
    return 1;
}

/* Add a rule for every path in a ':'-separated list. A path that contains ':'
   gets split and most likely fails to open, i.e. fails closed. */
static int add_rules(int ruleset, const char *paths, uint64_t rights)
{
    char path[4096];
    const char *p = paths;
    while (p != NULL && *p != '\0') {
        const char *end = strchr(p, ':');
        size_t len = end != NULL ? (size_t)(end - p) : strlen(p);
        if (len >= sizeof path) {
            errno = ENAMETOOLONG;
            fail("path is too long in", paths);
            return 0;
        }
        if (len > 0) {
            memcpy(path, p, len);
            path[len] = '\0';
            if (!add_rule(ruleset, path, rights))
                return 0;
        }
        p = end != NULL ? end + 1 : NULL;
    }
    return 1;
}

__attribute__((constructor))
static void rpki_prover_sandbox(void)
{
    const char *rw_env = getenv(ENV_RW);
    const char *ro_env = getenv(ENV_RO);
    if (rw_env == NULL && ro_env == NULL)
        return;

    /* Keep copies and drop the variables, so that they are not passed on
       to anything this process might start. */
    char rw[8192], ro[8192];
    snprintf(rw, sizeof rw, "%s", rw_env != NULL ? rw_env : "");
    snprintf(ro, sizeof ro, "%s", ro_env != NULL ? ro_env : "");
    const char *writes_only_env = getenv(ENV_WRITES_ONLY);
    int writes_only = writes_only_env != NULL && strcmp(writes_only_env, "1") == 0;
    unsetenv(ENV_RW);
    unsetenv(ENV_RO);
    unsetenv(ENV_WRITES_ONLY);

    if (!is_worker_process())
        return;

    int abi = (int) syscall(__NR_landlock_create_ruleset, NULL, 0, LL_CREATE_RULESET_VERSION);
    if (abi < 0) {
        /* EPERM is what a container's seccomp filter may answer with */
        if (errno == ENOSYS || errno == EOPNOTSUPP || errno == EPERM) {
            sandbox_status = SANDBOX_UNSUPPORTED;
            snprintf(sandbox_message, sizeof sandbox_message,
                     "Landlock is not available: %s", strerror(errno));
        } else {
            fail("landlock_create_ruleset(version)", NULL);
        }
        return;
    }
    sandbox_abi = abi;

    preload_libraries();

    uint64_t fs_rights = fs_rights_for_abi(abi, writes_only);
    struct ll_ruleset_attr attr = {
        .handled_access_fs  = fs_rights,
        .handled_access_net = abi >= 4 && !writes_only ? (LL_NET_BIND_TCP | LL_NET_CONNECT_TCP) : 0,
        .scoped             = abi >= 6 && !writes_only ? (LL_SCOPE_ABSTRACT_UNIX_SOCKET | LL_SCOPE_SIGNAL) : 0
    };

    int ruleset = (int) syscall(__NR_landlock_create_ruleset, &attr, sizeof attr, 0);
    if (ruleset < 0) {
        fail("landlock_create_ruleset", NULL);
        return;
    }

    /* Reading isn't restricted with writes_only, so there's nothing to allow */
    if (add_rules(ruleset, rw, fs_rights) &&
        (writes_only || add_rules(ruleset, ro, LL_FS_READ_FILE | LL_FS_READ_DIR))) {

        if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0)
            fail("prctl(PR_SET_NO_NEW_PRIVS)", NULL);
        else if (syscall(__NR_landlock_restrict_self, ruleset, 0) != 0)
            fail("landlock_restrict_self", NULL);
        else
            sandbox_status = SANDBOX_APPLIED;
    }
    close(ruleset);
}

#else

/* Not Linux: record that the sandbox was asked for but can't be provided */
__attribute__((constructor))
static void rpki_prover_sandbox(void)
{
    if (getenv("RPKI_PROVER_SANDBOX_RW") != NULL || getenv("RPKI_PROVER_SANDBOX_RO") != NULL) {
        sandbox_status = SANDBOX_UNSUPPORTED;
        snprintf(sandbox_message, sizeof sandbox_message,
                 "Landlock is only available on Linux");
    }
}

#endif
