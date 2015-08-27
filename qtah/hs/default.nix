{ cabal
, binary
, cppop
, qtah-cpp
, qtah-generator
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:

cabal.mkDerivation (self: {
  pname = "qtah";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ binary cppop ];
  extraLibraries = [ qtah-cpp ];

  prePatch = ''
    ${qtah-generator}/bin/qtah-generator --gen-hs src
  '';

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;

  meta = {
    homepage = "http://khumba.net/projects/qtah";
    description = "Qt bindings for Haskell";
    license = self.stdenv.lib.licenses.agpl3;
    platforms = self.ghc.meta.platforms;
  };
} // (if enableSplitObjs != null then { inherit enableSplitObjs; } else {}))
