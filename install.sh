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

# Builds and installs Qtah.  See --help.

set -euo pipefail
projectDir=$(readlink -f "$0")
projectDir=$(dirname "$projectDir")
declare -r projectDir
. "$projectDir/common.sh"

usage() {
    cat <<EOF
install.sh - Qtah build script

Builds and installs Qtah for a specific version of Qt's API.  Performs an
incremental build, unless clean.sh is run first.  Some environment variables
control this script's operation:

  QTAH_BUILD_JOBS:
    This may be a positive integer, to control how many build jobs are run in
    parallel.  Passed as --jobs to cabal build and used directly by qtah-cpp.

  QTAH_QT_FLAGS:
    This value is passed as --flags to the qtah-cpp and qtah packages, and can
    be used to set the qtX flags (e.g. qt4, qt5).

  QTAH_QT and QT_SELECT and be used to select Qt versions.  See README.md for
  more information.
EOF
}

if [[ ${1:-} = --help ]]; then
    usage
    exit 0
fi

commands=" $* "

sdist() {
    if [[ $commands = *\ sdist\ * ]]; then
        run cabal sdist
    fi
}

echo
msg "Building and installing qtah-generator."
goToPkg qtah-generator
run cabal configure
run cabal build ${QTAH_BUILD_JOBS:+--jobs="$QTAH_BUILD_JOBS"}
run cabal install --force-reinstalls
sdist

echo
msg "Building and installing qtah-cpp."
goToPkg qtah-cpp
run cabal configure ${QTAH_QT_FLAGS:+--flags="$QTAH_QT_FLAGS"}
run cabal build ${QTAH_BUILD_JOBS:+--jobs="$QTAH_BUILD_JOBS"}
run cabal install ${QTAH_QT_FLAGS:+--flags="$QTAH_QT_FLAGS"} --force-reinstalls
sdist

echo
msg "Building and installing qtah."
goToPkg qtah
run cabal configure ${QTAH_QT_FLAGS:+--flags="$QTAH_QT_FLAGS"} \
    --enable-tests --enable-executable-dynamic
run cabal build ${QTAH_BUILD_JOBS:+--jobs="$QTAH_BUILD_JOBS"}
run cabal test
# Haddock spews out many thousands of lines about undocumented items, so we
# silence them.
run cabal haddock --haddock-options=--no-print-missing-docs
run cabal install ${QTAH_QT_FLAGS:+--flags="$QTAH_QT_FLAGS"} \
    --enable-tests --enable-executable-dynamic --force-reinstalls
sdist

if [[ $commands = *\ examples\ * ]] || [[ $commands = *\ sdist\ * ]]; then
    echo
    msg "Building qtah-examples."
    goToPkg qtah-examples
    run cabal configure --enable-executable-dynamic
    run cabal build
    sdist
fi
