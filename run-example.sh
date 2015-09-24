#!/usr/bin/env bash

# Make sure that Qtah is built (build.sh) and that the Cabal package is
# installed (cd qtah/hs && cabal install) before running this script.

set -euo pipefail
declare -r projectDir="$(dirname "$(readlink -f "$0")")"
. "$projectDir/common.sh"

cd "$projectDir/qtah-examples"
run cabal configure
run cabal build
run dist/build/qtah-examples/qtah-examples "$@"
