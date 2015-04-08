#!/usr/bin/env bash

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

cd "$projectDir/lang/hs"
LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${buildDir}" \
    cabal run qtpi-demo -- "$@"
