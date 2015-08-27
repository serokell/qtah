Nix expressions are provided to ease building Qtah within
[Nixpkgs](https://nixos.org/nixpkgs).  Qtah and Cppop can be inserted into
Nixpkgs with the following overrides, after updating `cppopDir` and `qtahDir` as
appropriate for your environment.  Disabling GHC split objects and enabling
parallel building speeds up build time during development.

    packageOverrides =
      let cppopDir = /my/projects/cppop.git;
          qtahDir = /my/projects/qtah.git;
          cppopQtahOverrides = {
            enableSplitObjs = false;  # default null
            forceParallelBuilding = true;  # default false
          }; in pkgs: rec {
      qtah-cpp = pkgs.callPackage (qtahDir + /qtah/cpp) {
        qtah-generator = haskellPackages.qtah-generator;
      };

      haskellPackages = pkgs.haskellPackages.override {
        extension = self: super: {
          cppop = self.callPackage cppopDir cppopQtahOverrides;
          qtah-generator = self.callPackage (qtahDir + /qtah-generator) cppopQtahOverrides;
          qtah = self.callPackage (qtahDir + /qtah/hs) cppopQtahOverrides;
          qtah-examples = self.callPackage (qtahDir + /qtah-examples) cppopQtahOverrides;
        };
      };
