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

# Cleans up generated build outputs.

set -euo pipefail
projectDir=$(readlink -f "$0")
projectDir=$(dirname "$projectDir")
declare -r projectDir
. "$projectDir/common.sh"

usage() {
    cat <<EOF
clean.sh - Qtah build clean-up script

Removes all build outputs created by building Qtah.
EOF
}

if [[ ${1:-} = --help ]]; then
    usage
    exit 0
fi

run cd "$projectDir/qtah"
run cabal clean

run cd "$projectDir/qtah-generator"
run cabal clean

run cd "$projectDir/qtah-examples"
run cabal clean
