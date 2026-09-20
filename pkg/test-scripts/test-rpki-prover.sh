#!/bin/bash
#
# Install / upgrade / remove / purge tests for the rpki-prover Debian package.
#
# Must run as root on a throwaway machine with systemd (see pkg/vagrant).
#
#   test-rpki-prover.sh --deb rpki-prover_0.9.9-1~deb12u1_amd64.deb all
#   test-rpki-prover.sh --deb NEW.deb --old-deb OLD.deb upgrade
#
# Exit status is non-zero if any check fails.

set -uo pipefail

DEB=""
OLD_DEB=""
SERVICE_TIMEOUT=${SERVICE_TIMEOUT:-120}
HTTP_PORT=${HTTP_PORT:-9999}
MODES=()

FAILURES=0
CHECKS=0

usage() {
    cat >&2 <<'USAGE'
Usage: test-rpki-prover.sh --deb PATH [--old-deb PATH] MODE...

Modes:
  fresh-install   install onto a clean system and verify the service works
  upgrade         install --old-deb, then --deb over it (needs --old-deb)
  remove          apt-get remove and verify what survives
  purge           apt-get purge and verify nothing survives
  all             fresh-install, remove, purge (and upgrade if --old-deb given)

Options:
  --deb PATH            package under test (required)
  --old-deb PATH        older package, for the upgrade test
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
        --deb)             DEB=$(readlink -f "$2"); shift 2 ;;
        --old-deb)         OLD_DEB=$(readlink -f "$2"); shift 2 ;;
        --service-timeout) SERVICE_TIMEOUT=$2; shift 2 ;;
        --http-port)       HTTP_PORT=$2; shift 2 ;;
        -h|--help)         usage; exit 0 ;;
        -*)                echo "Unknown option: $1" >&2; usage; exit 1 ;;
        *)                 MODES+=("$1"); shift ;;
    esac
done

[ "$(id -u)" -eq 0 ] || { echo "Must run as root" >&2; exit 1; }
[ -n "${DEB}" ] || { echo "--deb is required" >&2; usage; exit 1; }
[ -f "${DEB}" ] || { echo "No such file: ${DEB}" >&2; exit 1; }
[ ${#MODES[@]} -gt 0 ] || MODES=(all)

if [ "${MODES[0]}" = "all" ]; then
    if [ -n "${OLD_DEB}" ]; then
        MODES=(fresh-install remove purge upgrade remove purge)
    else
        MODES=(fresh-install remove purge)
    fi
fi

export DEBIAN_FRONTEND=noninteractive

# --- reusable assertions ---------------------------------------------------

install_deb() {  # install_deb <path>
    info "apt-get install $(basename "$1")"
    apt-get install -y --no-install-recommends "$1" >/tmp/apt-install.log 2>&1 || {
        fail "apt-get install $(basename "$1")"
        tail -40 /tmp/apt-install.log >&2
        return 1
    }
    pass "apt-get install $(basename "$1")"
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
        # Not "journalctl | grep -q": grep exits at the first match, journalctl
        # is killed by SIGPIPE, and under `set -o pipefail` the pipeline reports
        # 141 even though the line was there. The bigger the journal, the more
        # likely that is, so the check would fail exactly when it matters.
        if grep -qE "$1" <<< "$(journalctl -u rpki-prover --no-pager)"; then return 0; fi
        sleep 2
        i=$((i + 2))
    done
    return 1
}

dump_service_state() {
    info "--- systemctl status ---"
    systemctl status rpki-prover --no-pager --full 2>&1 | sed 's/^/       /' >&2 || true
    info "--- last 60 journal lines ---"
    journalctl -u rpki-prover --no-pager -n 60 2>&1 | sed 's/^/       /' >&2 || true
}

assert_installed_state() {
    check "binary /usr/bin/rpki-prover is executable" test -x /usr/bin/rpki-prover
    check "man page is installed" test -f /usr/share/man/man1/rpki-prover.1.gz
    check "conffile /etc/default/rpki-prover exists" test -f /etc/default/rpki-prover
    check "systemd unit is installed" test -f /lib/systemd/system/rpki-prover.service
    check "user rpki-prover exists" getent passwd rpki-prover
    check "group rpki-prover exists" getent group rpki-prover
    check "state directory exists" test -d /var/lib/rpki-prover
    check "tals directory exists" test -d /var/lib/rpki-prover/tals
    check "rsync was pulled in" test -x /usr/bin/rsync

    check_eq "state directory is owned by rpki-prover" \
        "rpki-prover rpki-prover" "$(stat -c '%U %G' /var/lib/rpki-prover)"

    local version
    version=$(/usr/bin/rpki-prover --version 2>/dev/null)
    if printf '%s' "${version}" | grep -q '^rpki-prover-'; then
        pass "rpki-prover --version works (${version})"
    else
        fail "rpki-prover --version works (got '${version}')"
    fi

    check_eq "conffile is registered with dpkg" \
        "yes" \
        "$(dpkg-query -W -f='${Conffiles}' rpki-prover | grep -qc '/etc/default/rpki-prover' && echo yes || echo no)"
}

assert_service_healthy() {
    check_eq "service is enabled" "enabled" "$(systemctl is-enabled rpki-prover 2>&1)"

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

    # A crash loop shows up as restarts; the service should have needed none.
    check_eq "service did not restart" "0" "$(systemctl show -p NRestarts --value rpki-prover)"

    # On a first start rpki-prover downloads the RIR TALs into its state
    # directory; without them the daemon runs but validates nothing. The HTTP
    # API comes up in parallel with the validator, so this has to be waited
    # for rather than checked the moment the port answers.
    if wait_for_journal 'Successfully loaded [1-9][0-9]* TALs'; then
        pass "TALs were loaded"
    else
        fail "TALs were loaded"
        journalctl -u rpki-prover --no-pager -n 30 | sed 's/^/       /' >&2
    fi

    if [ -n "$(find /var/lib/rpki-prover/tals -name '*.tal' 2>/dev/null)" ]; then
        pass "TAL files were downloaded into the state directory"
    else
        fail "TAL files were downloaded into the state directory"
    fi
}

# --- modes -----------------------------------------------------------------

mode_fresh_install() {
    step "fresh-install"
    check_not "rpki-prover is not installed to begin with" dpkg -s rpki-prover
    install_deb "${DEB}" || return
    assert_installed_state
    assert_service_healthy
}

mode_upgrade() {
    step "upgrade"
    [ -n "${OLD_DEB}" ] || { fail "upgrade test needs --old-deb"; return; }

    install_deb "${OLD_DEB}" || return
    local old_version
    old_version=$(dpkg-query -W -f='${Version}' rpki-prover)
    info "installed old version ${old_version}"
    wait_for_active || info "old version did not become active; continuing anyway"

    # A locally modified conffile must survive the upgrade untouched.
    echo '# local modification by the test suite' >> /etc/default/rpki-prover
    sed -i 's/--cpu-count 2/--cpu-count 1/' /etc/default/rpki-prover

    install_deb "${DEB}" || return
    local new_version
    new_version=$(dpkg-query -W -f='${Version}' rpki-prover)
    info "upgraded to ${new_version}"

    if dpkg --compare-versions "${new_version}" gt "${old_version}"; then
        pass "version increased (${old_version} -> ${new_version})"
    else
        fail "version increased (${old_version} -> ${new_version})"
    fi

    check "local conffile modification survived" \
        grep -q 'local modification by the test suite' /etc/default/rpki-prover
    check "conffile edit survived" grep -q -- '--cpu-count 1' /etc/default/rpki-prover
    check_not "no .dpkg-dist file was left behind" test -f /etc/default/rpki-prover.dpkg-dist

    check_eq "service is still enabled" "enabled" "$(systemctl is-enabled rpki-prover 2>&1)"
    if wait_for_active; then
        pass "service is running after the upgrade"
    else
        fail "service is running after the upgrade"
        dump_service_state
    fi

    check "cache directory survived the upgrade" test -d /var/lib/rpki-prover
}

mode_remove() {
    step "remove"
    info "apt-get remove rpki-prover"
    apt-get remove -y rpki-prover >/tmp/apt-remove.log 2>&1 \
        || { fail "apt-get remove"; tail -40 /tmp/apt-remove.log >&2; return; }
    pass "apt-get remove"

    check_not "service is no longer active" systemctl is-active --quiet rpki-prover
    check_not "binary is gone" test -e /usr/bin/rpki-prover
    check_not "systemd unit is gone" test -e /lib/systemd/system/rpki-prover.service

    # Removal, unlike purge, must keep configuration and data.
    check "conffile is kept on remove" test -f /etc/default/rpki-prover
    check "state directory is kept on remove" test -d /var/lib/rpki-prover
    check "user is kept on remove" getent passwd rpki-prover

    check_eq "package is in config-files state" \
        "config-files" "$(dpkg-query -W -f='${db:Status-Status}' rpki-prover 2>/dev/null)"
}

mode_purge() {
    step "purge"
    info "apt-get purge rpki-prover"
    apt-get purge -y rpki-prover >/tmp/apt-purge.log 2>&1 \
        || { fail "apt-get purge"; tail -40 /tmp/apt-purge.log >&2; return; }
    pass "apt-get purge"

    check_not "package is gone" dpkg -s rpki-prover
    check_not "conffile is gone" test -e /etc/default/rpki-prover
    check_not "state directory is gone" test -e /var/lib/rpki-prover
    check_not "user is gone" getent passwd rpki-prover
    check_not "group is gone" getent group rpki-prover
}

# --- run -------------------------------------------------------------------

apt-get update >/dev/null 2>&1 || true
apt-get install -y --no-install-recommends curl procps >/dev/null 2>&1 || true

for mode in "${MODES[@]}"; do
    case "${mode}" in
        fresh-install) mode_fresh_install ;;
        upgrade)       mode_upgrade ;;
        remove)        mode_remove ;;
        purge)         mode_purge ;;
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
