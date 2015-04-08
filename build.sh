#!/usr/bin/env bash

# Accepts MAKEOPTS.

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

echo
msg "Building Cppop."
"$cppopProjectDir/build.sh"

echo
msg "Installing the Cppop Haskell bits."
"$cppopProjectDir/install-haskell.sh"

echo
msg "Generating bindings."
run mkdir -p "$projectDir/lang/hs/src/Foreign/Cppop/Generated"
run cd "$projectDir/lang/hs"
run cabal configure
run cabal build qtpi-generator
run dist/build/qtpi-generator/qtpi-generator \
    --gen-cpp-cpp "$projectDir/lang/cpp/bindings.cpp" \
    --gen-cpp-h "$projectDir/lang/cpp/bindings.hpp" \
    --gen-hs "$projectDir/lang/hs/src/Foreign/Cppop/Generated/Qtpi.hs"

echo
msg "Building the C++ library."
if ! [[ -d $buildDir ]]; then
    run mkdir "$buildDir"
fi
run cd "$buildDir"
run qmake "$projectDir/lang/cpp/qtpi.pro"
run make ${MAKEOPTS:-}

echo
msg "Building the Haskell bindings."
run cd "$projectDir/lang/hs"
run cabal configure
run cabal build
