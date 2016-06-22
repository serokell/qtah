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

{ stdenv, qt, qtah-generator, lib }:
let
  qtVersionComponents = lib.strings.splitString "." qt.version;
  qtMajor = builtins.elemAt qtVersionComponents 0;
in stdenv.mkDerivation {
  name = "qtah-cpp-0.1.0";
  src = ./.;
  buildInputs = [ qt qtah-generator ];
  enableParallelBuilding = true;

  prePatch = ''
    ${qtah-generator}/bin/qtah-generator --gen-cpp .
    ${qtah-generator}/bin/qtah-listener-gen --gen-cpp-dir .
  '';
  preConfigure = "qmake PREFIX=$out qtah.pro";

  meta = {
    homepage = "http://khumba.net/projects/qtah";
    description = "C++ library for Qtah bindings";
    license = stdenv.lib.licenses.lgpl3Plus;
    platforms = stdenv.lib.platforms.unix;
  };
}
