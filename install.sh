#!/usr/bin/env bash

# This file is part of Qtah.
#
# Copyright 2015-2018 The Qtah Authors.
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
if [[ $(uname) = Darwin ]] && which greadlink >/dev/null 2>&1; then
    readlink() { greadlink "$@"; }
fi
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
    parallel.  Passed as --jobs to cabal build.

  QTAH_QT_FLAGS:
    This value is passed as --flags to the qtah-cpp and qtah packages, and can
    be used to set the qtX flags (e.g. qt4, qt5) for Qt version selection.
    If unset, this variable defaults to qt5.  If it set to the empty string,
    then the system default Qt is used.  See README.md for more information.

  QTAH_QMAKE:
    This overrides the QMake executable used to build qtah-cpp.
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

# Default to qt5 if no version preference has been specified.
: ${QTAH_QT_FLAGS=qt5}

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
run cabal configure ${QTAH_QT_FLAGS:+--flags="$QTAH_QT_FLAGS"} --enable-tests
run cabal build ${QTAH_BUILD_JOBS:+--jobs="$QTAH_BUILD_JOBS"}
# We call the test executable directly instead of running 'cabal test', for
# distros such as Debian Stretch that are affected by
# https://github.com/haskell/cabal/issues/2438.
#run env LD_LIBRARY_PATH=$PWD/dist/build dist/build/test-qtah/test-qtah
# 2017-03-22: Switch back to 'cabal test' for debugging tests not running on
# GHC-8.0.2:
run cabal test
# Haddock spews out many thousands of lines about undocumented items, so we
# silence them.
run cabal haddock --haddock-options=--no-print-missing-docs
run cabal install ${QTAH_QT_FLAGS:+--flags="$QTAH_QT_FLAGS"} \
    --enable-tests --force-reinstalls
sdist

if [[ $commands = *\ examples\ * ]] || [[ $commands = *\ sdist\ * ]]; then
    echo
    msg "Building qtah-examples."
    goToPkg qtah-examples
    run cabal configure
    run cabal build ${QTAH_BUILD_JOBS:+--jobs="$QTAH_BUILD_JOBS"}
    sdist
fi
