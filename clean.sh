#!/usr/bin/env bash

set -euo pipefail
declare -r projectDir="$(dirname "$(realpath "$0")")"
. "$projectDir/common.sh"

run cd "$projectDir/qtah/hs"
run cabal clean
run rm -rf "$projectDir/qtah/cpp-build"
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Generated"
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Core/Q"*
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Widgets/Q"*

run cd "$projectDir/qtah-generator"
run cabal clean

run cd "$projectDir/qtah-examples"
run cabal clean
