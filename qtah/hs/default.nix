{ mkDerivation, base, binary, bytestring, cppop, stdenv, lib
, qtah-generator
, qtah-cpp
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "qtah";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base binary bytestring cppop ];
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
