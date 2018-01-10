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

{ mkDerivation, base, binary, bytestring, hoppy-runtime
# Keep qtah on a line by itself, scripts/set-qt-version looks for it:
, qtah
, stdenv, lib
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "qtah-examples";
  version = "0.3.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring hoppy-runtime
    # Keep qtah on a line by itself, scripts/set-qt-version looks for it:
    qtah  # dep
  ];
  homepage = "http://khumba.net/projects/qtah";
  description = "Example programs for Qtah Qt bindings";
  license = stdenv.lib.licenses.lgpl3Plus;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
