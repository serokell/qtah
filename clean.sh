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

# Cleans up generated build outputs.

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
clean.sh - Qtah build clean-up script

Removes all build outputs created by building Qtah.
EOF
}

if [[ ${1:-} = --help ]]; then
    usage
    exit 0
fi

for pkg in qtah-generator qtah-cpp qtah qtah-examples; do
    for variant in "" -qt4 -qt5; do
        p=${pkg}${variant}
        if [[ -d ${projectDir}/${pkg}${variant} ]]; then
            run cd "${projectDir}/${pkg}${variant}"
            run cabal clean
        fi
    done
done
