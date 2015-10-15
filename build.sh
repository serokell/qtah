#!/usr/bin/env bash

# Accepts MAKEOPTS.

set -euo pipefail
declare -r projectDir="$(dirname "$(readlink -f "$0")")"
. "$projectDir/common.sh"

run "$projectDir/tools/listener-gen.sh" \
    --gen-cpp-dir "$projectDir/qtah/cpp" \
    --gen-hs-dir "$projectDir/qtah-generator"

echo
msg "Generating bindings."
run mkdir -p "$projectDir/qtah/hs/src/Foreign/Hoppy/Generated"
run cd "$projectDir/qtah-generator"
run cabal configure
run cabal build
run dist/build/qtah-generator/qtah-generator \
    --gen-cpp "$projectDir/qtah/cpp" \
    --gen-hs "$projectDir/qtah/hs/src"

echo
msg "Building the C++ library."
if ! [[ -d $cppBuildDir ]]; then
    run mkdir "$cppBuildDir"
fi
run cd "$cppBuildDir"
run qmake "$projectDir/qtah/cpp/qtah.pro"
run make ${MAKEOPTS:-}

echo
msg "Building the Haskell bindings."
run cd "$projectDir/qtah/hs"
run cabal configure --extra-lib-dirs="$projectDir/qtah/cpp-build"
run cabal build
