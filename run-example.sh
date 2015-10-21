#!/usr/bin/env bash

# This file is part of Qtah.
#
# Copyright 2015 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License version 3
# as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Make sure that Qtah is built (build.sh) and that the Cabal package is
# installed (cd qtah/hs && cabal install) before running this script.

set -euo pipefail
declare -r projectDir="$(dirname "$(readlink -f "$0")")"
. "$projectDir/common.sh"

cd "$projectDir/qtah-examples"
run cabal configure
run cabal build
run dist/build/qtah-examples/qtah-examples "$@"
