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

{ mkDerivation, base, binary, bytestring, hoppy, stdenv, lib
, qtah-generator
, qtah-cpp
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "qtah";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base binary bytestring hoppy ];
  librarySystemDepends = [ qtah-cpp ];
  homepage = "http://khumba.net/projects/qtah";
  description = "Qt bindings for Haskell";
  license = stdenv.lib.licenses.agpl3;

  prePatch = ''
    ${qtah-generator}/bin/qtah-generator --gen-hs src
  '';

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
