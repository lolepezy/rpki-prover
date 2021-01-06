#!/bin/sh

# Script for running a lot of validators at once to catch weird bugs.

COMMAND="${HOME}/.local/bin/rpki-prover --revalidation-interval 400 --rrdp-refresh-interval 120 --rsync-refresh-interval 600 --repository-grace-period 1000 --cache-lifetime-hours 72"

for I in {1..20}; do 
    RPKI_ROOT=${HOME}/tmp/rpki-${I}
    mkdir -p ${RPKI_ROOT}
    # ./mkdirs.sh ${RPKI_ROOT}
    HTTP_PORT=$((19999 + $I))
    # echo $HTTP_PORT
    CMD="${COMMAND} --rpki-root-directory ${RPKI_ROOT} --http-api-port ${HTTP_PORT}"
    # Make them a little not syncronous

    echo "Executing $CMD"
    sh -c "$CMD > ${HOME}/tmp/log-$I 2>${HOME}/tmp/error.log-$I" &
    sleep 10
done


