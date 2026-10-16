#!/bin/ksh
#
# Runs inside the OpenBSD VM as root, on every build; each step is a no-op once
# done. Gives the box room to build in and turns it into a ports build host.
# Upgrading to the target release happens before this, in build-package.sh,
# since it takes reboots.

set -eu

RELEASE=$1

# --- Disk -------------------------------------------------------------------
#
# The second disk (see the Vagrantfile) becomes /usr/local and /build. The
# contents of the box's own /usr/local are copied over first.
if ! mount | grep -q ' on /build '; then
    if ! disklabel sd1 2>/dev/null | grep -q '^  b:'; then
        fdisk -iy sd1 >/dev/null
        printf '/usr/local 20G\n/build 1G-*\n' > /tmp/sd1.template
        disklabel -w -A -T /tmp/sd1.template sd1
        newfs -q /dev/rsd1a >/dev/null
        newfs -q /dev/rsd1b >/dev/null
        mkdir -p /mnt/usrlocal
        mount /dev/sd1a /mnt/usrlocal
        (cd /usr/local && tar cpf - .) | (cd /mnt/usrlocal && tar xpf -)
        umount /mnt/usrlocal
    fi
    duid=$(sysctl -n hw.disknames | sed -E 's/.*sd1:([0-9a-f]+).*/\1/')
    sed -i '/ \/usr\/local /s/^/#/' /etc/fstab
    echo "${duid}.a /usr/local ffs rw,wxallowed,nodev 1 2" >> /etc/fstab
    echo "${duid}.b /build ffs rw,wxallowed,nodev 1 2" >> /etc/fstab
    mkdir -p /build
    mount /usr/local
    mount /build
fi

# --- Packages ---------------------------------------------------------------

# An rpki-prover left installed by a test that did not finish would make the
# update fail: the mirror knows no such package.
if pkg_info -e 'rpki-prover-*' >/dev/null; then
    rcctl stop rpki_prover >/dev/null 2>&1 || true
    pkg_delete -c rpki-prover
fi
pkg_add -u -I
pkg_add -I ghc cabal-install cabal-bundler rsync-- git

# --- Ports tree -------------------------------------------------------------

if [ "$(cat /build/ports/.release 2>/dev/null)" != "${RELEASE}" ]; then
    rm -rf /build/ports /build/ports.tar.gz
    cd /build
    ftp -V -o ports.tar.gz "https://cdn.openbsd.org/pub/OpenBSD/${RELEASE}/ports.tar.gz"
    ftp -V -o SHA256.sig "https://cdn.openbsd.org/pub/OpenBSD/${RELEASE}/SHA256.sig"
    signify -C -p "/etc/signify/openbsd-$(echo "${RELEASE}" | tr -d .)-base.pub" \
        -x SHA256.sig ports.tar.gz
    tar xzf ports.tar.gz
    rm ports.tar.gz SHA256.sig
    echo "${RELEASE}" > /build/ports/.release
fi
ln -sfn /build/ports /usr/ports

# Where build-package.sh uploads the port and the source tarball, as the
# unprivileged ssh user.
rm -rf /build/upload
install -d -o "${SUDO_USER:-root}" /build/upload

# Keep everything the ports tree writes on the big disk.
mkdir -p /build/distfiles /build/pobj /build/packages /build/plist /build/bulk
chown -R _pfetch:wheel /build/distfiles
chown -R _pbuild:wheel /build/pobj /build/packages /build/plist /build/bulk
cat > /etc/mk.conf <<'EOF'
DISTDIR=/build/distfiles
WRKOBJDIR=/build/pobj
PACKAGE_REPOSITORY=/build/packages
PLIST_REPOSITORY=/build/plist
BULK_COOKIES_DIR=/build/bulk
EOF
