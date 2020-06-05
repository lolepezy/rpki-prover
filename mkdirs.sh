#!/bin/sh

mkdir -p ${HOME}/.rpki/cache
mkdir -p ${HOME}/.rpki/tmp
mkdir -p ${HOME}/.rpki/rsync
mkdir -p ${HOME}/.rpki/tals
mkdir -p ${HOME}/.rpki/tals

cd ${HOME}/.rpki/tals

wget https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/afrinic.tal
wget https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/apnic.tal
wget https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/arin.tal
wget https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/lacnic.tal
wget https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/ripe.tal
