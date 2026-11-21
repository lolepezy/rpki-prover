#!/bin/sh
#
# Generates the TA certificates used by RPKI.Validation.TaCertSpec to test
# RFC 10097 (TA tiebreaking): certificates of the same key with different
# validity periods, and one of a different key.
#
# Needs OpenSSL >= 3.4 (for -not_before/-not_after). The private keys are
# not kept, run it again to regenerate all of the certificates at once.
#
set -eu

cd "$(dirname "$0")"

KEYS=$(mktemp -d)
trap 'rm -rf "$KEYS"' EXIT

openssl genrsa -out "$KEYS/ta.key"    2048 2>/dev/null
openssl genrsa -out "$KEYS/other.key" 2048 2>/dev/null

cat > "$KEYS/ext.cnf" <<'EOF'
[ req ]
distinguished_name = dn
prompt             = no

[ dn ]
CN = rfc10097-test-ta

[ ta ]
basicConstraints       = critical, CA:TRUE
keyUsage               = critical, keyCertSign, cRLSign
subjectKeyIdentifier   = hash
authorityKeyIdentifier = none
certificatePolicies    = critical, 1.3.6.1.5.5.7.14.2
sbgp-ipAddrBlock       = critical, IPv4:0.0.0.0/0, IPv6:::/0
sbgp-autonomousSysNum  = critical, AS:0-4294967295
subjectInfoAccess      = caRepository;URI:rsync://rpki.example.net/repo/, 1.3.6.1.5.5.7.48.10;URI:rsync://rpki.example.net/repo/ta.mft, 1.3.6.1.5.5.7.48.13;URI:https://rpki.example.net/notification.xml
EOF

# name key serial notBefore notAfter
cert() {
    openssl req -new -x509 -sha256 \
        -key "$KEYS/$2.key" \
        -config "$KEYS/ext.cnf" -extensions ta \
        -set_serial "$3" \
        -not_before "$4" -not_after "$5" \
        -outform DER -out "$1.cer"
}

cert ta-2020-2100           ta    1 20200101000000Z 21000101000000Z
cert ta-2020-2100-reissued  ta    2 20200101000000Z 21000101000000Z
cert ta-2021-2100           ta    3 20210101000000Z 21000101000000Z
cert ta-2020-2090           ta    4 20200101000000Z 20900101000000Z
cert ta-2000-2010           ta    5 20000101000000Z 20100101000000Z
cert ta-2090-2100           ta    6 20900101000000Z 21000101000000Z
cert ta-other-key           other 7 20200101000000Z 21000101000000Z
