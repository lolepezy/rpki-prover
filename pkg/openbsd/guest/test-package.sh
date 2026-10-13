#!/bin/ksh
#
# Runs inside a throwaway OpenBSD VM as root: install, run, restart and remove
# checks for the rpki-prover package.
#
#   test-package.sh PACKAGE.tgz [SIGNIFY.pub]
#
# Without a public key the package is installed with -D unsigned. Exit status
# is non-zero if any check fails.

set -u

PKG=$1
PUBKEY=${2:-}
STATE=/var/db/rpki-prover
SERVICE_TIMEOUT=${SERVICE_TIMEOUT:-180}

FAILURES=0
CHECKS=0

pass() { CHECKS=$((CHECKS + 1)); echo "  ok   $1"; }
fail() { CHECKS=$((CHECKS + 1)); FAILURES=$((FAILURES + 1)); echo "  FAIL $1"; }
step() { echo; echo "--- $1"; }

check() {
    desc=$1; shift
    if "$@" >/dev/null 2>&1; then pass "${desc}"; else fail "${desc}"; fi
}

check_not() {
    desc=$1; shift
    if "$@" >/dev/null 2>&1; then fail "${desc}"; else pass "${desc}"; fi
}

check_eq() {
    if [ "$2" = "$3" ]; then pass "$1"; else fail "$1 (expected '$2', got '$3')"; fi
}

wait_for() {    # wait_for <seconds> <command...>
    t=$1; shift
    while [ "${t}" -gt 0 ]; do
        "$@" >/dev/null 2>&1 && return 0
        sleep 2; t=$((t - 2))
    done
    return 1
}

http_up() { ftp -o /dev/null -V http://127.0.0.1:9999/; }
tal_count() { ls "${STATE}"/tals/*.tal 2>/dev/null | wc -l | tr -d ' '; }
five_tals() { [ "$(tal_count)" -eq 5 ]; }

step "Clean slate"
rcctl stop rpki_prover >/dev/null 2>&1
pkg_delete -c rpki-prover >/dev/null 2>&1
rm -rf "${STATE}"
check_not "no rpki-prover installed" pkg_info -e 'rpki-prover-*'

step "Install"
if [ -n "${PUBKEY}" ]; then
    rm -f "/etc/signify/$(basename "${PUBKEY}")"
    check_not "pkg_add refuses it without the key" pkg_add -I "${PKG}"
    cp "${PUBKEY}" /etc/signify/
    check "pkg_add of the signed package" pkg_add "${PKG}"
else
    check "pkg_add -D unsigned" pkg_add -D unsigned "${PKG}"
fi
check "binary installed" test -x /usr/local/bin/rpki-prover
check "binary runs" /usr/local/bin/rpki-prover --version
check "rsync is there" test -x /usr/local/bin/rsync
check "rc.d script installed" test -x /etc/rc.d/rpki_prover
check "user _rpki-prover exists" id _rpki-prover
check_eq "uid of _rpki-prover" 906 "$(id -u _rpki-prover)"
check_eq "state directory owner" "_rpki-prover:_rpki-prover" "$(stat -f %Su:%Sg "${STATE}")"
check_eq "state directory mode" "drwxr-x---" "$(stat -f %Sp "${STATE}")"
check "login class installed" test -f /etc/login.conf.d/rpki_prover
check_eq "rc.d picks up the login class" rpki_prover "$(rcctl get rpki_prover class)"
check_not "service not enabled by the install" sh -c 'rcctl ls on | grep -qx rpki_prover'

step "Start"
check "rcctl enable" rcctl enable rpki_prover
check "rcctl start" rcctl start rpki_prover
check "rcctl check" rcctl check rpki_prover
check "HTTP API answers within ${SERVICE_TIMEOUT}s" wait_for "${SERVICE_TIMEOUT}" http_up
check "five TALs downloaded" wait_for 60 five_tals
# By uid: ps cuts user names down to 8 characters.
check_eq "daemon runs as _rpki-prover" 906 \
    "$(ps -o uid= -p "$(pgrep -xf "/usr/local/bin/rpki-prover --rpki-root-directory ${STATE} --http-api-port 9999")" | tr -d ' ')"
check_eq "datasize limit of the daemon (KB)" 8388608 \
    "$(su -c rpki_prover -s /bin/sh _rpki-prover -c 'ulimit -d')"
check "logs reach syslog" wait_for 30 grep -q 'rpki_prover\[[0-9]*\]: Info ' /var/log/daemon
check "workers spawned" wait_for 60 sh -c "[ \$(pgrep -u _rpki-prover -f -- '--worker' | wc -l) -gt 0 ]"

step "Restart"
check "rcctl restart" rcctl restart rpki_prover
check "HTTP API answers after restart" wait_for "${SERVICE_TIMEOUT}" http_up

step "Stop"
check "rcctl stop" rcctl stop rpki_prover
check "no rpki-prover processes left" wait_for 60 sh -c '! pgrep -u _rpki-prover >/dev/null'
check_not "rcctl check fails when stopped" rcctl check rpki_prover

step "Remove"
check "rcctl disable" rcctl disable rpki_prover
check "pkg_delete" pkg_delete rpki-prover
check_not "binary gone" test -e /usr/local/bin/rpki-prover
check_not "rc.d script gone" test -e /etc/rc.d/rpki_prover
check "state directory kept" test -d "${STATE}/tals"
check "user kept" id _rpki-prover

step "Reinstall over the old state, then remove with -c"
if [ -n "${PUBKEY}" ]; then pkg_add "${PKG}" >/dev/null; else pkg_add -D unsigned "${PKG}" >/dev/null; fi
check "TALs survived reinstall" five_tals
check "pkg_delete -c" pkg_delete -c rpki-prover
check_not "state directory removed" test -e "${STATE}"
check_not "user removed" id _rpki-prover
check_not "login class removed" test -e /etc/login.conf.d/rpki_prover

echo
echo "${CHECKS} checks, ${FAILURES} failed"
[ "${FAILURES}" -eq 0 ]
