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

{ stdenv, qt4, qtah-generator }:
stdenv.mkDerivation {
  name = "qtah-cpp-0.1.0";
  src = ./.;
  buildInputs = [ qt4 qtah-generator ];
  enableParallelBuilding = true;

  prePatch = ''
    ${qtah-generator}/bin/qtah-generator --gen-cpp .
    ${qtah-generator}/bin/qtah-listener-gen --gen-cpp-dir .
  '';
  preConfigure = "qmake PREFIX=$out qtah.pro";

  meta = {
    homepage = "http://khumba.net/projects/qtah";
    description = "C++ library for Qtah bindings";
    license = stdenv.lib.licenses.agpl3;
    platforms = stdenv.lib.platforms.unix;
  };
}
