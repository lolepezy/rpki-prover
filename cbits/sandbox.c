/*
 * Sandboxing for worker processes and for the external programs they run.
 *
 * What happens is decided by the command line:
 *
 *   rpki-prover --worker ...
 *       Apply the sandbox described by the variables below (if any of them is
 *       set) and carry on into the GHC runtime. The worker's Haskell code then
 *       asks how it went (see RPKI.Sandbox), reports it and refuses to run if
 *       the sandbox was asked for but couldn't be set up.
 *
 *   rpki-prover --sandboxed-exec PROGRAM ARGS...
 *       Launcher for external programs (the rsync client). Close inherited
 *       descriptors, set resource limits, apply the sandbox and exec PROGRAM
 *       (an absolute path) in place of this process. The GHC runtime never
 *       starts, and PROGRAM keeps the PID, the standard descriptors and the
 *       parent of this process, so to the caller it looks as if it had started
 *       PROGRAM directly. If anything goes wrong the launcher writes one line
 *       to stderr, doesn't run PROGRAM and exits with LAUNCHER_SETUP_FAILED or
 *       LAUNCHER_EXEC_FAILED.
 *
 * Both are needed because Landlock only applies to the thread that asks for it
 * and to threads and processes created after that. A threaded RTS already
 * runs a dozen OS threads by the time any Haskell code can do anything, so a
 * worker has to do it in a constructor, while the process is still
 * single-threaded. And a worker can't restrict just the process it's about to
 * start, only itself, so it starts the launcher, which restricts itself
 * further (Landlock rules stack, every layer can only take access away) and
 * becomes the program.
 *
 * Variables:
 *
 *   RPKI_PROVER_SANDBOX_RW    ':'-separated paths with full read/write access
 *   RPKI_PROVER_SANDBOX_RO    ':'-separated paths with read-only access
 *   RPKI_PROVER_SANDBOX_EXEC  ':'-separated paths that can be read and executed
 *   RPKI_PROVER_SANDBOX_TCP   "none" (the default), "any" or ','-separated
 *                             ports that TCP connections are allowed to
 *   RPKI_PROVER_SANDBOX_REQUIRED  "1" for the launcher to refuse to run
 *                             PROGRAM where Landlock is not available
 *   RPKI_PROVER_LIMIT_CPU     launcher only, RLIMIT_CPU in seconds
 *   RPKI_PROVER_LIMIT_AS      launcher only, RLIMIT_AS in bytes
 *   RPKI_PROVER_LIMIT_FSIZE   launcher only, RLIMIT_FSIZE in bytes
 *   RPKI_PROVER_LIMIT_NOFILE  launcher only, RLIMIT_NOFILE
 *
 * If any of the SANDBOX_RW/RO/EXEC/TCP variables is set, everything else on
 * the filesystem is denied, as are TCP bind and connect, abstract unix
 * sockets and signals to processes outside of the sandbox (as far as the
 * kernel's Landlock version supports these). A path that doesn't exist makes
 * the sandbox fail, a path that contains ':' gets split and most likely
 * doesn't exist, i.e. fails closed too.
 *
 * Every process removes all RPKI_PROVER_* variables from its environment,
 * so that they are not passed on to anything it starts.
 *
 * Nothing is written to stderr in the worker case -- the parent expects only
 * encoded log messages there.
 */

#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/stat.h>

/* Keep in sync with RPKI.Sandbox */
#define SANDBOX_NOT_REQUESTED 0
#define SANDBOX_APPLIED       1
#define SANDBOX_UNSUPPORTED   2
#define SANDBOX_FAILED        3

#define LAUNCHER_SETUP_FAILED 117
#define LAUNCHER_EXEC_FAILED  118

#define WORKER_OPTION   "--worker"
#define LAUNCHER_OPTION "--sandboxed-exec"

#define ENV_PREFIX       "RPKI_PROVER_"
#define ENV_RW           "RPKI_PROVER_SANDBOX_RW"
#define ENV_RO           "RPKI_PROVER_SANDBOX_RO"
#define ENV_EXEC         "RPKI_PROVER_SANDBOX_EXEC"
#define ENV_TCP          "RPKI_PROVER_SANDBOX_TCP"
#define ENV_REQUIRED     "RPKI_PROVER_SANDBOX_REQUIRED"
#define ENV_LIMIT_CPU    "RPKI_PROVER_LIMIT_CPU"
#define ENV_LIMIT_AS     "RPKI_PROVER_LIMIT_AS"
#define ENV_LIMIT_FSIZE  "RPKI_PROVER_LIMIT_FSIZE"
#define ENV_LIMIT_NOFILE "RPKI_PROVER_LIMIT_NOFILE"

extern char **environ;

static int  sandbox_status = SANDBOX_NOT_REQUESTED;
static int  sandbox_abi    = 0;
static char sandbox_message[1024];

int         rpki_prover_sandbox_status(void)  { return sandbox_status; }
int         rpki_prover_sandbox_abi(void)     { return sandbox_abi; }
const char *rpki_prover_sandbox_message(void) { return sandbox_message; }

/* What the variables ask for, copied before they are removed */
struct sandbox_request {
    char *rw;
    char *ro;
    char *exec;
    char *tcp;
    int   required;
};

static char *copy_variable(const char *name)
{
    const char *value = getenv(name);
    return value != NULL ? strdup(value) : NULL;
}

/* Returns whether a sandbox is requested at all */
static int read_request(struct sandbox_request *r)
{
    const char *required = getenv(ENV_REQUIRED);
    r->rw       = copy_variable(ENV_RW);
    r->ro       = copy_variable(ENV_RO);
    r->exec     = copy_variable(ENV_EXEC);
    r->tcp      = copy_variable(ENV_TCP);
    r->required = required != NULL && strcmp(required, "1") == 0;
    return r->rw != NULL || r->ro != NULL || r->exec != NULL || r->tcp != NULL;
}

static void drop_our_variables(void)
{
    /* unsetenv changes environ, so start over after every removal. The bound
       is there just in case unsetenv doesn't remove what it's asked to. */
    for (int attempts = 0; attempts < 1000; attempts++) {
        char **e = environ;
        while (*e != NULL && strncmp(*e, ENV_PREFIX, sizeof ENV_PREFIX - 1) != 0)
            e++;
        if (*e == NULL)
            return;
        const char *eq = strchr(*e, '=');
        char *name = strndup(*e, eq != NULL ? (size_t)(eq - *e) : strlen(*e));
        int rc = name != NULL ? unsetenv(name) : -1;
        free(name);
        if (rc != 0)
            return;
    }
}

/* ------------------------------------------------------------------------ */
/* Landlock                                                                 */
/* ------------------------------------------------------------------------ */

#if defined(__linux__)

#include <stdint.h>
#include <sys/prctl.h>
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
#ifndef __NR_close_range
#define __NR_close_range 436
#endif
#ifndef O_PATH
#define O_PATH 010000000
#endif

/* From <linux/landlock.h>, which may be missing or too old */
#define LL_CREATE_RULESET_VERSION (1U << 0)
#define LL_RULE_PATH_BENEATH      1
#define LL_RULE_NET_PORT          2

#define LL_FS_EXECUTE      (1ULL << 0)
#define LL_FS_WRITE_FILE   (1ULL << 1)
#define LL_FS_READ_FILE    (1ULL << 2)
#define LL_FS_READ_DIR     (1ULL << 3)
#define LL_FS_REFER        (1ULL << 13)
#define LL_FS_TRUNCATE     (1ULL << 14)
#define LL_FS_IOCTL_DEV    (1ULL << 15)
/* EXECUTE .. MAKE_SYM, i.e. everything Landlock ABI 1 knows about */
#define LL_FS_ABI_1        ((1ULL << 13) - 1)
/* Rights that make sense for a file rather than a directory */
#define LL_FS_FILE_RIGHTS  (LL_FS_EXECUTE | LL_FS_WRITE_FILE | LL_FS_READ_FILE | \
                            LL_FS_TRUNCATE | LL_FS_IOCTL_DEV)
#define LL_FS_READ         (LL_FS_READ_FILE | LL_FS_READ_DIR)

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

struct ll_net_port_attr {
    uint64_t allowed_access;
    uint64_t port;
} __attribute__((packed));

static void fail(const char *what, const char *detail)
{
    int e = errno;
    sandbox_status = SANDBOX_FAILED;
    if (detail != NULL)
        snprintf(sandbox_message, sizeof sandbox_message, "%s %s: %s", what, detail, strerror(e));
    else
        snprintf(sandbox_message, sizeof sandbox_message, "%s: %s", what, strerror(e));
}

/* The Landlock ABI version, or -1 with errno set */
static int landlock_abi(void)
{
    return (int) syscall(__NR_landlock_create_ruleset, NULL, 0, LL_CREATE_RULESET_VERSION);
}

int rpki_prover_landlock_abi(void)
{
    int abi = landlock_abi();
    return abi >= 0 ? abi : -errno;
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

static uint64_t fs_rights_for_abi(int abi)
{
    uint64_t rights = LL_FS_ABI_1;
    if (abi >= 2) rights |= LL_FS_REFER;
    if (abi >= 3) rights |= LL_FS_TRUNCATE;
    if (abi >= 5) rights |= LL_FS_IOCTL_DEV;
    return rights;
}

static int add_path_rule(int ruleset, const char *path, uint64_t rights)
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

/* Add a rule for every path in a ':'-separated list */
static int add_path_rules(int ruleset, const char *paths, uint64_t rights)
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
            if (!add_path_rule(ruleset, path, rights))
                return 0;
        }
        p = end != NULL ? end + 1 : NULL;
    }
    return 1;
}

static int is_any_network(const char *tcp)
{
    return tcp != NULL && strcmp(tcp, "any") == 0;
}

/* Allow connecting to every port in a ','-separated list */
static int add_port_rules(int ruleset, const char *ports)
{
    if (ports == NULL || strcmp(ports, "none") == 0)
        return 1;

    const char *p = ports;
    while (*p != '\0') {
        char *end;
        errno = 0;
        unsigned long port = strtoul(p, &end, 10);
        if (end == p || errno != 0 || port > 65535 || (*end != ',' && *end != '\0')) {
            errno = EINVAL;
            fail("invalid port list", ports);
            return 0;
        }
        struct ll_net_port_attr rule = { .allowed_access = LL_NET_CONNECT_TCP, .port = port };
        if (syscall(__NR_landlock_add_rule, ruleset, LL_RULE_NET_PORT, &rule, 0) != 0) {
            fail("landlock_add_rule", "(port)");
            return 0;
        }
        p = *end == ',' ? end + 1 : end;
    }
    return 1;
}

/* Restrict this thread and everything it starts later. Sets sandbox_status
   and friends and returns sandbox_status. */
static int apply_landlock(const struct sandbox_request *r)
{
    int abi = landlock_abi();
    if (abi < 0) {
        /* EPERM is what a container's seccomp filter may answer with */
        if (errno == ENOSYS || errno == EOPNOTSUPP || errno == EPERM) {
            sandbox_status = SANDBOX_UNSUPPORTED;
            snprintf(sandbox_message, sizeof sandbox_message,
                     "Landlock is not available: %s", strerror(errno));
        } else {
            fail("landlock_create_ruleset(version)", NULL);
        }
        return sandbox_status;
    }
    sandbox_abi = abi;

    /* Below ABI 4 there are no network rules, and ports are then ignored:
       the caller finds out from the ABI version. */
    int restrict_network = abi >= 4 && !is_any_network(r->tcp);

    uint64_t fs_rights = fs_rights_for_abi(abi);
    struct ll_ruleset_attr attr = {
        .handled_access_fs  = fs_rights,
        .handled_access_net = restrict_network ? (LL_NET_BIND_TCP | LL_NET_CONNECT_TCP) : 0,
        .scoped             = abi >= 6 ? (LL_SCOPE_ABSTRACT_UNIX_SOCKET | LL_SCOPE_SIGNAL) : 0
    };

    int ruleset = (int) syscall(__NR_landlock_create_ruleset, &attr, sizeof attr, 0);
    if (ruleset < 0) {
        fail("landlock_create_ruleset", NULL);
        return sandbox_status;
    }

    if (add_path_rules(ruleset, r->rw, fs_rights) &&
        add_path_rules(ruleset, r->ro, LL_FS_READ) &&
        add_path_rules(ruleset, r->exec, LL_FS_READ | LL_FS_EXECUTE) &&
        (!restrict_network || add_port_rules(ruleset, r->tcp))) {

        if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0)
            fail("prctl(PR_SET_NO_NEW_PRIVS)", NULL);
        else if (syscall(__NR_landlock_restrict_self, ruleset, 0) != 0)
            fail("landlock_restrict_self", NULL);
        else
            sandbox_status = SANDBOX_APPLIED;
    }
    close(ruleset);
    return sandbox_status;
}

#else /* not Linux */

int rpki_prover_landlock_abi(void)
{
    return -ENOSYS;
}

static void preload_libraries(void) {}

static int apply_landlock(const struct sandbox_request *r)
{
    (void) r;
    sandbox_status = SANDBOX_UNSUPPORTED;
    snprintf(sandbox_message, sizeof sandbox_message, "Landlock is only available on Linux");
    return sandbox_status;
}

#endif

/* ------------------------------------------------------------------------ */
/* Command line                                                             */
/* ------------------------------------------------------------------------ */

struct command_line {
    int    argc;
    char **argv;
};

#if defined(__linux__)

/* musl doesn't pass argc/argv to constructors, so read them from procfs.
   Returns 0 if that's not possible. */
static int read_command_line(struct command_line *cl, int argc, char **argv)
{
    (void) argc;
    (void) argv;

    int fd = open("/proc/self/cmdline", O_RDONLY | O_CLOEXEC);
    if (fd < 0)
        return 0;

    size_t size = 0, capacity = 4096;
    char *buf = malloc(capacity + 1);
    for (;;) {
        if (buf == NULL) {
            close(fd);
            return 0;
        }
        ssize_t n = read(fd, buf + size, capacity - size);
        if (n < 0 && errno == EINTR)
            continue;
        if (n <= 0)
            break;
        size += (size_t) n;
        if (size == capacity) {
            capacity *= 2;
            char *bigger = realloc(buf, capacity + 1);
            if (bigger == NULL)
                free(buf);
            buf = bigger;
        }
    }
    close(fd);
    if (size == 0) {
        free(buf);
        return 0;
    }
    buf[size] = '\0';

    int count = 0;
    for (size_t i = 0; i < size; i++)
        if (buf[i] == '\0')
            count++;
    /* The last argument may lack its terminator if the process changed it */
    if (buf[size - 1] != '\0')
        count++;

    cl->argv = calloc((size_t) count + 1, sizeof(char *));
    if (cl->argv == NULL) {
        free(buf);
        return 0;
    }
    cl->argc = 0;
    for (char *p = buf; p < buf + size; p += strlen(p) + 1)
        cl->argv[cl->argc++] = p;
    cl->argv[cl->argc] = NULL;
    return 1;
}

#else

/* The other systems we build on (macOS) pass them to constructors */
static int read_command_line(struct command_line *cl, int argc, char **argv)
{
    if (argc < 1 || argv == NULL)
        return 0;
    cl->argc = argc;
    cl->argv = argv;
    return 1;
}

#endif

/* ------------------------------------------------------------------------ */
/* Launcher                                                                 */
/* ------------------------------------------------------------------------ */

__attribute__((noreturn, format(printf, 1, 2)))
static void launcher_fail(const char *format, ...)
{
    char message[1200];
    va_list args;
    va_start(args, format);
    vsnprintf(message, sizeof message, format, args);
    va_end(args);
    dprintf(STDERR_FILENO, "rpki-prover " LAUNCHER_OPTION ": %s\n", message);
    _exit(LAUNCHER_SETUP_FAILED);
}

/* Don't let the program inherit whatever the worker had open (database
   files, sockets). Landlock needs kernel 5.13, close_range is there since
   5.9, the loop is for everything else. */
static void close_inherited_descriptors(void)
{
#if defined(__linux__)
    if (syscall(__NR_close_range, 3U, ~0U, 0U) == 0)
        return;
#endif
    struct rlimit rl;
    int max = 65536;
    if (getrlimit(RLIMIT_NOFILE, &rl) == 0 && rl.rlim_cur != RLIM_INFINITY && rl.rlim_cur < (rlim_t) max)
        max = (int) rl.rlim_cur;
    for (int fd = 3; fd < max; fd++)
        (void) close(fd);
}

static void set_limit(int resource, const char *name, const char *value, rlim_t extra_hard)
{
    if (value == NULL)
        return;

    char *end;
    errno = 0;
    unsigned long long v = strtoull(value, &end, 10);
    if (end == value || *end != '\0' || errno != 0)
        launcher_fail("invalid value '%s' of %s", value, name);

    struct rlimit current, limit = { .rlim_cur = (rlim_t) v, .rlim_max = (rlim_t) v + extra_hard };
    /* An unprivileged process can't raise the hard limit */
    if (getrlimit(resource, &current) == 0 && current.rlim_max != RLIM_INFINITY) {
        if (limit.rlim_cur > current.rlim_max) limit.rlim_cur = current.rlim_max;
        if (limit.rlim_max > current.rlim_max) limit.rlim_max = current.rlim_max;
    }
    if (setrlimit(resource, &limit) != 0)
        launcher_fail("cannot set %s to %s: %s", name, value, strerror(errno));
}

static void run_launcher(struct command_line *cl)
{
    if (cl->argc < 3 || cl->argv[2][0] != '/')
        launcher_fail("expected an absolute path of the program to run");

    struct sandbox_request request;
    int requested = read_request(&request);
    char *limit_cpu    = copy_variable(ENV_LIMIT_CPU);
    char *limit_as     = copy_variable(ENV_LIMIT_AS);
    char *limit_fsize  = copy_variable(ENV_LIMIT_FSIZE);
    char *limit_nofile = copy_variable(ENV_LIMIT_NOFILE);
    drop_our_variables();

    close_inherited_descriptors();

    /* SIGXCPU at the soft limit, SIGKILL a bit later if it's ignored */
    set_limit(RLIMIT_CPU,    ENV_LIMIT_CPU,    limit_cpu,    5);
    set_limit(RLIMIT_AS,     ENV_LIMIT_AS,     limit_as,     0);
    set_limit(RLIMIT_FSIZE,  ENV_LIMIT_FSIZE,  limit_fsize,  0);
    set_limit(RLIMIT_NOFILE, ENV_LIMIT_NOFILE, limit_nofile, 0);
    set_limit(RLIMIT_CORE,   "RLIMIT_CORE",    "0",          0);

    if (requested) {
        switch (apply_landlock(&request)) {
        case SANDBOX_APPLIED:
            break;
        case SANDBOX_UNSUPPORTED:
            if (request.required)
                launcher_fail("sandbox is required but %s", sandbox_message);
            break;
        default:
            launcher_fail("cannot set up sandbox: %s", sandbox_message);
        }
    }

    execv(cl->argv[2], cl->argv + 2);
    dprintf(STDERR_FILENO, "rpki-prover " LAUNCHER_OPTION ": cannot execute %s: %s\n",
            cl->argv[2], strerror(errno));
    _exit(LAUNCHER_EXEC_FAILED);
}

/* ------------------------------------------------------------------------ */

static int has_option(const struct command_line *cl, const char *option)
{
    return cl->argc >= 2 && strcmp(cl->argv[1], option) == 0;
}

__attribute__((constructor))
static void rpki_prover_sandbox(int argc, char **argv, char **envp)
{
    (void) envp;

    struct command_line cl = { 0, NULL };
    int known = read_command_line(&cl, argc, argv);

    if (known && has_option(&cl, LAUNCHER_OPTION))
        run_launcher(&cl);

    struct sandbox_request request;
    int requested = read_request(&request);
    drop_our_variables();

    /* If the command line can't be read, the variables are an explicit
       request, so honour them. */
    if (!requested || (known && !has_option(&cl, WORKER_OPTION)))
        return;

    preload_libraries();
    (void) apply_landlock(&request);
}
