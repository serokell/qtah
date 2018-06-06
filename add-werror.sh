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

# This script inserts the GHC option -Werror into all Cabal files in the
# project.  This change shouldn't be committed, but it's useful during
# development to keep warning-free, since build output can be lengthly.

set -euo pipefail
myDir=$(readlink -f "$0")
myDir=$(dirname "$myDir")
cd "$myDir"

sed -i 's/-W -f/-W -Werror -f/g' $(git grep -l -e '-W -f')
