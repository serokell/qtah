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

{ mkDerivation, base, containers, directory, filepath, haskell-src
, hoppy-generator, hoppy-std, mtl, stdenv, lib
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "qtah-generator";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory filepath haskell-src hoppy-generator
    hoppy-std mtl
  ];
  homepage = "http://khumba.net/projects/qtah";
  description = "Generator for Qtah Qt bindings";
  license = stdenv.lib.licenses.lgpl3Plus;

  preConfigure = ''
    ${if forceParallelBuilding
     then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
     else ""}
  '';
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
