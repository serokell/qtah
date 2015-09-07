{ mkDerivation, base, containers, cppop, directory, filepath
, haskell-src, mtl, stdenv, lib
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
    base containers cppop directory filepath haskell-src mtl
  ];
  homepage = "http://khumba.net/projects/qtah";
  description = "Generator for Qtah Qt bindings";
  license = stdenv.lib.licenses.agpl3;

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
