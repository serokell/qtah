{ cabal, cppop, filepath, haskellSrc, mtl
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:

cabal.mkDerivation (self: rec {
  pname = "qtah-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ cppop filepath haskellSrc mtl ];
  listenerGen = ../tools/listener-gen.sh;

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

  meta = {
    homepage = "http://khumba.net/projects/qtah";
    description = "Generator for Qtah Qt bindings";
    license = self.stdenv.lib.licenses.agpl3;
    platforms = self.ghc.meta.platforms;
  };
} // (if enableSplitObjs != null then { inherit enableSplitObjs; } else {}))
