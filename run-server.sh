#!/usr/bin/env bash

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

cd "$buildDir"

if ! [[ -p $serverInFifo ]]; then
    if [[ -e $serverInFifo ]]; then
        echo "Server input FIFO already exists ($serverInFifo), aborting."
        exit 1
    fi
    mkfifo /tmp/serverin
fi

if ! [[ -p $serverOutFifo ]]; then
    if [[ -e $serverOutFifo ]]; then
        echo "Server output FIFO already exists ($serverOutFifo), aborting."
        exit 1
    fi
    mkfifo /tmp/serverout
fi

[[ -f $serverLogFile ]] && rm "$serverLogFile"
LD_LIBRARY_PATH="$cppopObjDir" ./qtpi \
    --in "$serverInFifo" \
    --out "$serverOutFifo" \
    --log "$serverLogFile" \
    --threads 1
