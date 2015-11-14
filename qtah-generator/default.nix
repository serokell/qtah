# This file is part of Qtah.
#
# Copyright 2015 Bryan Gardiner <bog@khumba.net>
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
, hoppy, mtl, stdenv, lib
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:

let listenerGen = ../tools/listener-gen.sh; in

mkDerivation ({
  pname = "qtah-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory filepath haskell-src hoppy mtl
  ];
  homepage = "http://khumba.net/projects/qtah";
  description = "Generator for Qtah Qt bindings";
  license = stdenv.lib.licenses.lgpl3Plus;

  prePatch = ''
    ${listenerGen} --gen-hs-dir .
  '';

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;

  postInstall = ''
    install -T ${listenerGen} $out/bin/qtah-listener-gen
  '';
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
