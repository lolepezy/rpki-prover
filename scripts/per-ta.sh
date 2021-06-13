#!/bin/sh

# Script for running a lot of validators at once to catch weird bugs.

COMMAND="./rpki-prover --revalidation-interval 400 --cache-lifetime-hours 72 --cpu-count 8 +RTS -s -RTS"

PORT=29999

for TAL in apnic arin lacnic afrinic; do 

    RPKI_ROOT="${HOME}/tmp/rpki/${TAL}"    
    cp "${HOME}/.local/bin/rpki-prover" ${RPKI_ROOT}    

    # echo $HTTP_PORT
    CMD="${RPKI_ROOT}/${COMMAND} --rpki-root-directory ${RPKI_ROOT} --http-api-port ${PORT}"
    # Make them a little not syncronous

    echo "Executing $CMD"
    sh -c "$CMD > ${RPKI_ROOT}/log 2>${RPKI_ROOT}/error.log" &
    PORT=$((PORT + 1))
done


