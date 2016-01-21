#!/usr/bin/env bash

# This file is part of Qtah.
#
# Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
run cabal configure --flags="${QTAH_QT_FLAG:-qt54}"
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
