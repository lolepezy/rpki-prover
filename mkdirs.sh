#!/bin/sh

DEFAULT_ROOT=${HOME}/.rpki/

ROOT=${1:-$DEFAULT_ROOT}

echo "Creating directory layout in $ROOT"

mkdir -p ${ROOT}/cache
mkdir -p ${ROOT}/tmp
mkdir -p ${ROOT}/rsync
mkdir -p ${ROOT}/tals

curl -s https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/afrinic.tal > ${ROOT}/tals/afrinic.tal
curl -s https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/apnic.tal > ${ROOT}/tals/apnic.tal
curl -s https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/arin.tal > ${ROOT}/tals/arin.tal
curl -s https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/lacnic.tal > ${ROOT}/tals/lacnic.tal
curl -s https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/ripe.tal > ${ROOT}/tals/ripe.tal
