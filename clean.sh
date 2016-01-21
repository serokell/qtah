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

set -euo pipefail
declare -r projectDir="$(dirname "$(readlink -f "$0")")"
. "$projectDir/common.sh"

run cd "$projectDir/qtah/hs"
run cabal clean
run rm -rf "$projectDir/qtah/cpp/b_"*.{cpp,hpp}
run rm -rf "$projectDir/qtah/cpp/"{callback,listener}.{cpp,hpp}
run rm -rf "$projectDir/qtah/cpp-build"
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Generated"
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Core/Q"*
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Core/Types.hs"*
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Gui/Q"*
run rm -rf "$projectDir/qtah/hs/src/Graphics/UI/Qtah/Widgets/Q"*

run cd "$projectDir/qtah-generator"
run cabal clean
run rm -rf "$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs"
run rm -rf "$projectDir/qtah-generator/src/Graphics/UI/Qtah/Internal/Interface/Listener.hs-boot"

run cd "$projectDir/qtah-examples"
run cabal clean
