#!/usr/bin/env bash

set -euo pipefail
declare -r projectDir="$(dirname "$(realpath "$0")")"
. "$projectDir/common.sh"

cd "$projectDir/lang/hs"
run cabal build qtah-demo
LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${buildDir}" \
    run dist/build/qtah-demo/qtah-demo "$@"
