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
build.sh - Qtah build script

Builds and installs Qtah for a specific version of Qt's API.  Performs an
incremental build, unless clean.sh is run first.  Some environment variables
control this script's operation:

  QTAH_QT_FLAG (required):

    Specifies a version of the Qt API to generate bindings for.  This should be
    a string of the form "qtX_Y" for Qt version X.Y.  Currently all 4.x and 5.x
    versions are recognized by this script.

  QT_SELECT:

    For systems with qtchooser(1), this can be used to select which version of
    the Qt headers and libraries to build against.  If omitted, then your
    system's default Qt version will be used, and you should set a compatible
    QTAH_QT_FLAG.

  MAKEOPTS:

    Arguments in this string are passed along to 'make' for building the C++
    side of the bindings.
EOF
}

if [[ ${1:-} = --help ]]; then
    usage
    exit 0
fi

if ! [[ ${QTAH_QT_FLAG:-} = qt*_* ]]; then
    echo "build.sh: Please set QTAH_QT_FLAG.  See --help."
    exit 1
fi

echo
msg "Generating bindings."
run cd "$projectDir/qtah-generator"
run cabal configure --flags="${QTAH_QT_FLAG}"
run cabal build
run cabal install --flags="${QTAH_QT_FLAG}"

echo
msg "Building the Haskell bindings."
run cd "$projectDir/qtah"
run cabal configure
run cabal build
run cabal install
