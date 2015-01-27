#!/usr/bin/env bash

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

cd "$projectDir/lang/hs"
cabal run qtpi-demo -- "$@"
