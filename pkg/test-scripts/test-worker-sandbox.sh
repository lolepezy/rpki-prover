#!/bin/bash
#
# Checks that the packaged service can actually run its worker processes, and
# that the workers' Landlock sandbox survives the unit's hardening.
#
# test-rpki-prover.sh covers install/upgrade/remove/purge and that the daemon
# starts. This covers what happens afterwards: a validation cycle spawns worker
# processes that sandbox themselves with Landlock (see cbits/sandbox.c), and
# the unit's SystemCallFilter, PrivateDevices, ProtectSystem and friends are
# all in a position to break that. When they do, the worker either refuses to
# run or quietly runs unsandboxed, and neither shows up as a failed unit.
#
# Must run as root on a throwaway machine with systemd, with network access.
#
#   test-worker-sandbox.sh --deb rpki-prover_0.11.0-1~deb12u1_amd64.deb
#
# Exit status is non-zero if a worker failed, or if the workers ran without the
# sandbox they asked for.

set -uo pipefail

DEB=""
# A cold start downloads the TALs, then fetches the repositories before the
# first validation worker has anything to do.
WORKER_TIMEOUT=${WORKER_TIMEOUT:-600}
# The two worker kinds are sandboxed differently and the rsync one is the
# riskier: its policy restricts writing only, and the rsync client it runs
# inherits it. Rsync workers only appear once the first round reaches the
# repositories that need them, which takes a few minutes longer than the first
# validation worker.
RSYNC_TIMEOUT=${RSYNC_TIMEOUT:-900}

FAILURES=0
CHECKS=0

pass() { CHECKS=$((CHECKS + 1)); printf '  \033[32mok\033[0m   %s\n' "$1"; }
fail() { CHECKS=$((CHECKS + 1)); FAILURES=$((FAILURES + 1)); printf '  \033[31mFAIL\033[0m %s\n' "$1"; }
info() { printf '       %s\n' "$1"; }
step() { printf '\n\033[1m--- %s\033[0m\n' "$1"; }

while [ $# -gt 0 ]; do
    case "$1" in
        --deb)            DEB=$(readlink -f "$2"); shift 2 ;;
        --worker-timeout) WORKER_TIMEOUT=$2; shift 2 ;;
        --rsync-timeout)  RSYNC_TIMEOUT=$2; shift 2 ;;
        -h|--help)        echo "Usage: test-worker-sandbox.sh --deb PATH [--worker-timeout N] [--rsync-timeout N]" >&2; exit 0 ;;
        *)                echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

[ "$(id -u)" -eq 0 ] || { echo "Must run as root" >&2; exit 1; }
[ -n "${DEB}" ] && [ -f "${DEB}" ] || { echo "--deb PATH is required" >&2; exit 1; }

export DEBIAN_FRONTEND=noninteractive

journal() { journalctl -u rpki-prover --no-pager "$@" 2>/dev/null; }

# Not "journal | grep -q": grep exits at the first match, journalctl is killed
# by SIGPIPE, and under `set -o pipefail` the pipeline reports 141 even though
# the line was found. With a debug-level journal that is the common case, so
# every check below would report a failure that is not there.
journal_has() { grep -qE "$1" <<< "$(journal)"; }

# Waits for a line to appear in the journal, or gives up.
wait_for_journal() {  # wait_for_journal <regex> <timeout>
    local i=0
    while [ "${i}" -lt "$2" ]; do
        journal_has "$1" && return 0
        systemctl is-failed --quiet rpki-prover && return 1
        sleep 5
        i=$((i + 5))
    done
    return 1
}

step "install"
apt-get install -y --no-install-recommends "${DEB}" >/tmp/apt-install.log 2>&1 \
    || { fail "apt-get install"; tail -40 /tmp/apt-install.log >&2; exit 1; }
pass "apt-get install $(basename "${DEB}")"

step "wait for the first-start TAL download"
# The service is started by the postinst and downloads the RIR TALs on its
# first start. It must not be restarted while that is in flight: the files are
# created before they are written, and a restart in between leaves them empty.
# The next start then finds a non-empty tals/ directory, skips the download and
# dies parsing a zero-byte TAL, for good.
if wait_for_journal 'Successfully loaded [1-9][0-9]* TALs' 300; then
    pass "TALs were downloaded and loaded"
else
    fail "TALs were downloaded and loaded"
    journal -n 30 | sed 's/^/       /' >&2
fi

empty_tals=$(find /var/lib/rpki-prover/tals -name '*.tal' -empty 2>/dev/null | wc -l)
[ "${empty_tals}" -eq 0 ] \
    && pass "no empty TAL files were left behind" \
    || fail "no empty TAL files were left behind (${empty_tals} of them)"

step "enable debug logging"
# The sandbox reports itself at debug level, and "nothing complained" is not a
# check. This restart is safe now that the TALs are on disk.
sed -i 's|^RPKI_PROVER_ARGS="\(.*\)"|RPKI_PROVER_ARGS="\1 --log-level debug"|' /etc/default/rpki-prover
grep -q -- '--log-level debug' /etc/default/rpki-prover \
    && pass "--log-level debug set in /etc/default/rpki-prover" \
    || fail "--log-level debug set in /etc/default/rpki-prover"
systemctl restart rpki-prover

step "the unit permits the Landlock syscalls"
# @system-service only began to include @sandbox, which holds the landlock_*
# syscalls, in systemd 253. Anything older denies them unless they are named.
resolved=$(systemctl show -p SystemCallFilter --value rpki-prover | tr ' ' '\n' | grep -c '^landlock_')
info "systemd $(systemctl --version | head -1 | awk '{print $2}'), landlock syscalls in the unit's filter: ${resolved}/3"
if [ "${resolved}" -eq 3 ]; then
    pass "unit's SystemCallFilter allows landlock_create_ruleset/add_rule/restrict_self"
else
    fail "unit's SystemCallFilter allows the landlock_* syscalls (${resolved}/3)"
    info "without them the worker cannot sandbox itself and runs unsandboxed"
fi

step "workers run"
if wait_for_journal 'Worker (is sandboxed|is not sandboxed|could not sandbox)' "${WORKER_TIMEOUT}"; then
    pass "a worker process started within ${WORKER_TIMEOUT}s"
else
    fail "a worker process started within ${WORKER_TIMEOUT}s"
    info "--- last 40 journal lines ---"
    journal -n 40 | sed 's/^/       /' >&2
fi

step "worker sandbox"
if journal_has 'Worker is sandboxed, Landlock ABI'; then
    abi=$(journal | grep -oE 'Landlock ABI [0-9]+' | tail -1 | awk '{print $3}')
    pass "workers sandboxed themselves (Landlock ABI ${abi})"
else
    fail "workers sandboxed themselves"
fi

# Asked for a sandbox, could not have one, ran anyway. Only a warning in the
# log, the service stays green, and the sandbox is simply not there.
if journal_has 'Worker is not sandboxed'; then
    fail "no worker ran unsandboxed"
    journal | grep -E 'Worker is not sandboxed' | tail -3 | sed 's/^/       /' >&2
else
    pass "no worker ran unsandboxed"
fi

# Asked for a sandbox, could not have one, refused to run. Validation stops.
if journal_has 'could not sandbox itself'; then
    fail "no worker refused to run over its sandbox"
    journal | grep -E 'could not sandbox itself' | tail -3 | sed 's/^/       /' >&2
else
    pass "no worker refused to run over its sandbox"
fi

step "rsync fetch workers"
# The rsync worker's policy restricts writing only, and the rsync client it
# runs inherits it: if the policy is too narrow the client cannot write the
# mirror and the fetch fails without the sandbox ever being mentioned. These
# workers turn up on their own once the round reaches a repository that needs
# rsync, so this waits rather than forcing it. (--no-rrdp does not force it:
# it only filters a TA's initial publication points, and falls back to the
# unfiltered set when that leaves nothing.)
if wait_for_journal 'Running worker:.*rsync-fetch' "${RSYNC_TIMEOUT}"; then
    pass "an rsync fetch worker ran"

    if journal_has 'Running worker:.*rsync-fetch.*onlyRestrictWrites = True'; then
        pass "rsync fetch workers got the write-only sandbox"
    else
        fail "rsync fetch workers got the write-only sandbox"
    fi

    if journal_has 'RsyncProcessError'; then
        fail "the rsync client ran without errors"
        journal | grep -E 'RsyncProcessError' | tail -3 | sed 's/^/       /' >&2
    else
        pass "the rsync client ran without errors"
    fi

    if journal_has 'Fetched rsync://'; then
        pass "a repository was fetched over rsync inside the sandbox"
    else
        info "no rsync fetch finished yet; the client started but had not completed"
    fi
else
    info "no rsync fetch worker within ${RSYNC_TIMEOUT}s; that sandbox shape was not exercised"
fi

if journal_has "can't restrict network access"; then
    fail "workers' sandbox covers the network"
    journal | grep -E "can't restrict network access" | tail -2 | sed 's/^/       /' >&2
else
    pass "workers' sandbox covers the network"
fi

step "no worker failures"
# Every way RPKI.Worker reports a worker that did not finish on its own terms.
# SIGSYS is what a seccomp filter without SystemCallErrorNumber= would produce,
# SIGABRT is what glibc does when it cannot load libgcc_s inside the sandbox.
worker_trouble='died/killed|died with an exception|died in a strange way|execution timed out|was killed by|exited with code = |signal 31|SIGABRT|SIGSEGV'
if journal_has "${worker_trouble}"; then
    fail "no worker failures in the journal"
    journal | grep -E "${worker_trouble}" | tail -10 | sed 's/^/       /' >&2
else
    pass "no worker failures in the journal"
fi

# The daemon is meant to survive its workers; a restart loop would hide above.
restarts=$(systemctl show -p NRestarts --value rpki-prover)
[ "${restarts}" = "0" ] && pass "service did not restart" || fail "service did not restart (${restarts})"

step "validation actually progressed"
if journal_has "Validated TA '.*', got [0-9]+ VRPs"; then
    pass "a TA was validated end to end"
    journal | grep -E "Validated TA '.*', got [0-9]+ VRPs" | tail -3 | sed 's/^/       /'
else
    info "no completed validation cycle yet (not fatal: a cold start can take longer)"
fi

echo
if [ "${FAILURES}" -eq 0 ]; then
    printf '\033[32mAll %d sandbox checks passed.\033[0m\n' "${CHECKS}"
    exit 0
else
    printf '\033[31m%d of %d sandbox checks failed.\033[0m\n' "${FAILURES}" "${CHECKS}"
    exit 1
fi
