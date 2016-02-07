#!/usr/bin/env bash

# This file is part of Qtah.
#
# Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

# Installs an already compiled Qtah.  See --help.

set -euo pipefail
projectDir=$(readlink -f "$0")
projectDir=$(dirname "$projectDir")
declare -r projectDir
. "$projectDir/common.sh"

usage() {
    cat <<EOF
build.sh - Qtah install script

Installs Qtah after it has been compiled with build.sh.
EOF
}

if [[ ${1:-} = --help ]]; then
    usage
    exit 0
fi

cd "$projectDir/qtah/hs"
cabal install --extra-lib-dirs="$projectDir/qtah/cpp-build"
