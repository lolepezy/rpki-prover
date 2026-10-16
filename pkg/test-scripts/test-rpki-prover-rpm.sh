#!/bin/bash
#
# Install / upgrade / remove tests for the rpki-prover RPM package; the
# counterpart of test-rpki-prover.sh.
#
# Must run as root on a throwaway EL machine with systemd (see pkg/vagrant).
#
#   test-rpki-prover-rpm.sh --rpm rpki-prover-0.11.0-1.el9.x86_64.rpm all
#   test-rpki-prover-rpm.sh --rpm NEW.rpm --old-rpm OLD.rpm upgrade
#
# Where RPM differs from dpkg, the checks follow RPM conventions rather than
# the .deb's behaviour:
#
#   - the service is neither enabled nor started on install; %systemd_post
#     defers to the presets, which leave third-party services disabled;
#   - there is no purge: erasing keeps the system user and the cache, deletes
#     an unmodified config file and keeps a modified one as .rpmsave. The
#     "cleanup" mode removes what is left by hand, so that the next mode starts
#     from a clean machine.
#
# Exit status is non-zero if any check fails.

set -uo pipefail

RPM=""
OLD_RPM=""
SERVICE_TIMEOUT=${SERVICE_TIMEOUT:-120}
HTTP_PORT=${HTTP_PORT:-9999}
MODES=()

SYSCONFIG=/etc/sysconfig/rpki-prover
UNIT=/usr/lib/systemd/system/rpki-prover.service
STATE_DIR=/var/lib/rpki-prover

FAILURES=0
CHECKS=0

usage() {
    cat >&2 <<'USAGE'
Usage: test-rpki-prover-rpm.sh --rpm PATH [--old-rpm PATH] MODE...

Modes:
  fresh-install   install onto a clean system, enable the service, verify it works
  upgrade         install --old-rpm, then --rpm over it (needs --old-rpm)
  remove          dnf remove and verify what survives
  cleanup         remove what an erase leaves behind (user, cache, .rpmsave)
  all             fresh-install, remove, cleanup (and upgrade, remove, cleanup
                  if --old-rpm is given)

Options:
  --rpm PATH            package under test (required)
  --old-rpm PATH        older package, for the upgrade test
  --service-timeout N   seconds to wait for the service to come up (default 120)
  --http-port N         port the HTTP API is expected on (default 9999)
USAGE
}

# --- tiny test harness -----------------------------------------------------

pass() { CHECKS=$((CHECKS + 1)); printf '  \033[32mok\033[0m   %s\n' "$1"; }
fail() { CHECKS=$((CHECKS + 1)); FAILURES=$((FAILURES + 1)); printf '  \033[31mFAIL\033[0m %s\n' "$1"; }
info() { printf '       %s\n' "$1"; }
step() { printf '\n\033[1m--- %s\033[0m\n' "$1"; }

check() {   # check <description> <command...>
    local desc=$1; shift
    if "$@" >/dev/null 2>&1; then pass "${desc}"; else fail "${desc}"; fi
}

check_not() {
    local desc=$1; shift
    if "$@" >/dev/null 2>&1; then fail "${desc}"; else pass "${desc}"; fi
}

check_eq() {  # check_eq <description> <expected> <actual>
    if [ "$2" = "$3" ]; then pass "$1"; else fail "$1 (expected '$2', got '$3')"; fi
}

# --- argument parsing ------------------------------------------------------

while [ $# -gt 0 ]; do
    case "$1" in
        --rpm)             RPM=$(readlink -f "$2"); shift 2 ;;
        --old-rpm)         OLD_RPM=$(readlink -f "$2"); shift 2 ;;
        --service-timeout) SERVICE_TIMEOUT=$2; shift 2 ;;
        --http-port)       HTTP_PORT=$2; shift 2 ;;
        -h|--help)         usage; exit 0 ;;
        -*)                echo "Unknown option: $1" >&2; usage; exit 1 ;;
        *)                 MODES+=("$1"); shift ;;
    esac
done

[ "$(id -u)" -eq 0 ] || { echo "Must run as root" >&2; exit 1; }
[ -n "${RPM}" ] || { echo "--rpm is required" >&2; usage; exit 1; }
[ -f "${RPM}" ] || { echo "No such file: ${RPM}" >&2; exit 1; }
[ ${#MODES[@]} -gt 0 ] || MODES=(all)

if [ "${MODES[0]}" = "all" ]; then
    if [ -n "${OLD_RPM}" ]; then
        MODES=(fresh-install remove cleanup upgrade remove cleanup)
    else
        MODES=(fresh-install remove cleanup)
    fi
fi

# --- reusable assertions ---------------------------------------------------

# Journal lines from before this point belong to an earlier mode, and on a
# reinstall they would satisfy the checks below on their own.
JOURNAL_SINCE="@$(date +%s)"
mark_journal() { JOURNAL_SINCE="@$(date +%s)"; sleep 1; }
journal() { journalctl -u rpki-prover --no-pager --since "${JOURNAL_SINCE}" "$@" 2>/dev/null; }

evr_of_file() { rpm -qp --qf '%{EPOCHNUM}:%{VERSION}-%{RELEASE}' "$1" 2>/dev/null; }
evr_installed() { rpm -q --qf '%{EPOCHNUM}:%{VERSION}-%{RELEASE}' rpki-prover 2>/dev/null; }

install_rpm() {  # install_rpm <path>
    info "dnf install $(basename "$1")"
    dnf install -y --setopt=install_weak_deps=False "$1" >/tmp/dnf-install.log 2>&1 || {
        fail "dnf install $(basename "$1")"
        tail -40 /tmp/dnf-install.log >&2
        return 1
    }
    pass "dnf install $(basename "$1")"
}

wait_for_active() {
    local i=0
    while [ "${i}" -lt "${SERVICE_TIMEOUT}" ]; do
        if systemctl is-active --quiet rpki-prover; then return 0; fi
        if systemctl is-failed --quiet rpki-prover; then return 1; fi
        sleep 2
        i=$((i + 2))
    done
    return 1
}

wait_for_http() {
    local i=0
    while [ "${i}" -lt "${SERVICE_TIMEOUT}" ]; do
        if curl -fsS --max-time 5 "http://127.0.0.1:${HTTP_PORT}/" >/dev/null 2>&1; then
            return 0
        fi
        sleep 2
        i=$((i + 2))
    done
    return 1
}

wait_for_journal() {  # wait_for_journal <extended regex>
    local i=0
    while [ "${i}" -lt "${SERVICE_TIMEOUT}" ]; do
        # Not "journal | grep -q"; see test-rpki-prover.sh.
        if grep -qE "$1" <<< "$(journal)"; then return 0; fi
        sleep 2
        i=$((i + 2))
    done
    return 1
}

dump_service_state() {
    info "--- systemctl status ---"
    systemctl status rpki-prover --no-pager --full 2>&1 | sed 's/^/       /' >&2 || true
    info "--- last 60 journal lines ---"
    journal -n 60 | sed 's/^/       /' >&2 || true
}

assert_installed_state() {
    check "binary /usr/bin/rpki-prover is executable" test -x /usr/bin/rpki-prover
    check "man page is installed" test -f /usr/share/man/man1/rpki-prover.1.gz
    check "config ${SYSCONFIG} exists" test -f "${SYSCONFIG}"
    check "systemd unit is installed" test -f "${UNIT}"
    check "unit reads ${SYSCONFIG}" grep -q "^EnvironmentFile=-${SYSCONFIG}\$" "${UNIT}"
    check "sysusers.d file is installed" test -f /usr/lib/sysusers.d/rpki-prover.conf
    check "user rpki-prover exists" getent passwd rpki-prover
    check "group rpki-prover exists" getent group rpki-prover
    check "state directory exists" test -d "${STATE_DIR}"
    check "tals directory exists" test -d "${STATE_DIR}/tals"
    check "rsync was pulled in" test -x /usr/bin/rsync

    check_eq "state directory is owned by rpki-prover" \
        "rpki-prover rpki-prover" "$(stat -c '%U %G' "${STATE_DIR}")"

    local version
    version=$(/usr/bin/rpki-prover --version 2>/dev/null)
    if printf '%s' "${version}" | grep -q '^rpki-prover-'; then
        pass "rpki-prover --version works (${version})"
    else
        fail "rpki-prover --version works (got '${version}')"
    fi

    check "config file is marked %config with rpm" \
        bash -c "rpm -qc rpki-prover | grep -qx '${SYSCONFIG}'"
    check "the state directory belongs to the package" \
        bash -c "rpm -qf '${STATE_DIR}/tals' | grep -q '^rpki-prover-'"
}

# rpm -V, ignoring the config file when the test itself has edited it.
assert_rpm_verify() {  # assert_rpm_verify [ignore-config]
    local out
    out=$(rpm -V rpki-prover 2>&1)
    [ "${1:-}" = "ignore-config" ] && out=$(grep -v " ${SYSCONFIG}\$" <<< "${out}")
    if [ -z "${out}" ]; then
        pass "rpm -V finds every packaged file intact"
    else
        fail "rpm -V finds every packaged file intact"
        sed 's/^/       /' <<< "${out}" >&2
    fi
}

assert_service_healthy() {
    if wait_for_active; then
        pass "service became active within ${SERVICE_TIMEOUT}s"
    else
        fail "service became active within ${SERVICE_TIMEOUT}s"
        dump_service_state
        return
    fi

    local runas
    runas=$(systemctl show -p MainPID --value rpki-prover)
    if [ -n "${runas}" ] && [ "${runas}" != "0" ]; then
        check_eq "main process runs as rpki-prover" \
            "rpki-prover" "$(ps -o user= -p "${runas}" | tr -d ' ')"
    else
        fail "main process runs as rpki-prover (no MainPID)"
    fi

    if wait_for_http; then
        pass "HTTP API answers on port ${HTTP_PORT}"
    else
        fail "HTTP API answers on port ${HTTP_PORT}"
        dump_service_state
    fi

    check_eq "service did not restart" "0" "$(systemctl show -p NRestarts --value rpki-prover)"

    # See test-rpki-prover.sh: the TALs are downloaded on the first start, in
    # parallel with the HTTP API coming up.
    if wait_for_journal 'Successfully loaded [1-9][0-9]* TALs'; then
        pass "TALs were loaded"
    else
        fail "TALs were loaded"
        journal -n 30 | sed 's/^/       /' >&2
    fi

    if [ -n "$(find "${STATE_DIR}/tals" -name '*.tal' 2>/dev/null)" ]; then
        pass "TAL files were downloaded into the state directory"
    else
        fail "TAL files were downloaded into the state directory"
    fi
}

enable_service() {
    mark_journal
    if systemctl enable --now rpki-prover >/tmp/systemctl-enable.log 2>&1; then
        pass "systemctl enable --now rpki-prover"
    else
        fail "systemctl enable --now rpki-prover"
        sed 's/^/       /' /tmp/systemctl-enable.log >&2
    fi
}

# --- modes -----------------------------------------------------------------

mode_fresh_install() {
    step "fresh-install"
    check_not "rpki-prover is not installed to begin with" rpm -q rpki-prover
    check_not "user rpki-prover does not exist to begin with" getent passwd rpki-prover
    install_rpm "${RPM}" || return
    assert_installed_state
    assert_rpm_verify

    # The RPM convention, unlike Debian's: installing a service does not start it.
    check_eq "service is not enabled by the install" \
        "disabled" "$(systemctl is-enabled rpki-prover 2>&1)"
    check_not "service is not started by the install" systemctl is-active --quiet rpki-prover

    enable_service
    check_eq "service is enabled" "enabled" "$(systemctl is-enabled rpki-prover 2>&1)"
    assert_service_healthy
}

mode_upgrade() {
    step "upgrade"
    [ -n "${OLD_RPM}" ] || { fail "upgrade test needs --old-rpm"; return; }

    install_rpm "${OLD_RPM}" || return
    local old_evr
    old_evr=$(evr_installed)
    info "installed old version ${old_evr}"
    enable_service
    wait_for_active || info "old version did not become active; continuing anyway"
    local old_pid
    old_pid=$(systemctl show -p MainPID --value rpki-prover)

    # A locally modified config file must survive the upgrade untouched.
    echo '# local modification by the test suite' >> "${SYSCONFIG}"
    sed -i 's/--cpu-count 2/--cpu-count 1/' "${SYSCONFIG}"

    mark_journal
    install_rpm "${RPM}" || return
    local new_evr
    new_evr=$(evr_installed)
    info "upgraded to ${new_evr}"

    check_eq "the new package replaced the old one" "$(evr_of_file "${RPM}")" "${new_evr}"
    check_eq "only one version is installed" "1" "$(rpm -q rpki-prover | wc -l)"
    if [ "${new_evr}" != "${old_evr}" ]; then
        pass "version changed (${old_evr} -> ${new_evr})"
    else
        fail "version changed (${old_evr} -> ${new_evr})"
    fi

    check "local config modification survived" \
        grep -q 'local modification by the test suite' "${SYSCONFIG}"
    check "config edit survived" grep -q -- '--cpu-count 1' "${SYSCONFIG}"
    check_not "no .rpmnew file was left behind" test -f "${SYSCONFIG}.rpmnew"
    check_not "no .rpmsave file was left behind" test -f "${SYSCONFIG}.rpmsave"
    assert_rpm_verify ignore-config

    check_eq "service is still enabled" "enabled" "$(systemctl is-enabled rpki-prover 2>&1)"
    if wait_for_active; then
        pass "service is running after the upgrade"
    else
        fail "service is running after the upgrade"
        dump_service_state
    fi

    # %systemd_postun_with_restart: the running daemon must be the new binary,
    # not the old one carrying on from a deleted inode.
    local new_pid
    new_pid=$(systemctl show -p MainPID --value rpki-prover)
    if [ -n "${new_pid}" ] && [ "${new_pid}" != "0" ] && [ "${new_pid}" != "${old_pid}" ]; then
        pass "service was restarted by the upgrade"
    else
        fail "service was restarted by the upgrade (MainPID ${old_pid} -> ${new_pid})"
    fi
    check_eq "the running binary is the installed one" \
        "/usr/bin/rpki-prover" "$(readlink "/proc/${new_pid}/exe" 2>/dev/null)"

    if wait_for_http; then
        pass "HTTP API answers after the upgrade"
    else
        fail "HTTP API answers after the upgrade"
    fi
    check "state directory survived the upgrade" test -d "${STATE_DIR}/tals"
}

mode_remove() {
    step "remove"
    # Not "rpm -V | grep -q": rpm -V exits non-zero exactly when a file was
    # modified, and under pipefail that sinks the whole pipeline.
    local config_modified=0
    grep -q " ${SYSCONFIG}\$" <<< "$(rpm -V rpki-prover 2>/dev/null)" && config_modified=1

    info "dnf remove rpki-prover"
    dnf remove -y rpki-prover >/tmp/dnf-remove.log 2>&1 \
        || { fail "dnf remove"; tail -40 /tmp/dnf-remove.log >&2; return; }
    pass "dnf remove"

    check_not "package is gone" rpm -q rpki-prover
    check_not "service is no longer active" systemctl is-active --quiet rpki-prover
    check_not "service is no longer enabled" \
        test -e /etc/systemd/system/multi-user.target.wants/rpki-prover.service
    check_not "binary is gone" test -e /usr/bin/rpki-prover
    check_not "systemd unit is gone" test -e "${UNIT}"
    check_not "man page is gone" test -e /usr/share/man/man1/rpki-prover.1.gz

    if [ "${config_modified}" -eq 1 ]; then
        check_not "modified config file is moved aside" test -e "${SYSCONFIG}"
        check "modified config file is kept as .rpmsave" \
            grep -q -- '--cpu-count 1' "${SYSCONFIG}.rpmsave"
    else
        check_not "unmodified config file is gone" test -e "${SYSCONFIG}"
        check_not "no .rpmsave for an unmodified config file" test -e "${SYSCONFIG}.rpmsave"
    fi

    # RPM has no purge: the cache and the user are left for the admin.
    check "state directory is kept on remove" test -d "${STATE_DIR}"
    check "user is kept on remove" getent passwd rpki-prover
}

mode_cleanup() {
    step "cleanup (what a purge would do)"
    check_not "package is not installed" rpm -q rpki-prover
    rm -rf "${STATE_DIR}" "${SYSCONFIG}.rpmsave" "${SYSCONFIG}.rpmnew"
    getent passwd rpki-prover >/dev/null && userdel rpki-prover
    getent group rpki-prover >/dev/null && groupdel rpki-prover
    systemctl daemon-reload
    systemctl reset-failed rpki-prover >/dev/null 2>&1 || true

    check_not "user is gone" getent passwd rpki-prover
    check_not "group is gone" getent group rpki-prover
    check_not "state directory is gone" test -e "${STATE_DIR}"
    check_not "systemd no longer knows the unit" \
        bash -c "systemctl list-unit-files rpki-prover.service | grep -q rpki-prover"
}

# --- run -------------------------------------------------------------------

dnf install -y curl procps-ng >/dev/null 2>&1 || true

for mode in "${MODES[@]}"; do
    case "${mode}" in
        fresh-install) mode_fresh_install ;;
        upgrade)       mode_upgrade ;;
        remove)        mode_remove ;;
        cleanup)       mode_cleanup ;;
        *) echo "Unknown mode: ${mode}" >&2; exit 1 ;;
    esac
done

echo
if [ "${FAILURES}" -eq 0 ]; then
    printf '\033[32mAll %d checks passed.\033[0m\n' "${CHECKS}"
    exit 0
else
    printf '\033[31m%d of %d checks failed.\033[0m\n' "${FAILURES}" "${CHECKS}"
    exit 1
fi
