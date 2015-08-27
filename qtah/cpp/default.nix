{ stdenv, qt4, qtah-generator }:

stdenv.mkDerivation {
  name = "qtah-cpp";
  version = "0.1.0";
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
