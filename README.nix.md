Nix expressions are provided to ease building Qtah within
[Nixpkgs](https://nixos.org/nixpkgs).  Qtah can be inserted into Nixpkgs with
the following overrides, after updating `qtahDir` as appropriate for your
environment.  Cppop must already be at `haskellPackages.cppop`; if it's not,
follow the similar setup instructions for Cppop.

    packageOverrides = let qtahDir = /my/projects/qtah.git; in pkgs: rec {
      qtah-cpp = pkgs.callPackage (qtahDir + /qtah/cpp) {
        inherit (haskellPackages) qtah-generator;
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          qtah-generator = self.callPackage (qtahDir + /qtah-generator) {};
          qtah = self.callPackage (qtahDir + /qtah/hs) {};
          qtah-examples = self.callPackage (qtahDir + /qtah-examples) {};
        };
      };

The Haskell packages accept two optional parameters.  `enableSplitObjs`, when
non-null, will override Nixpkgs's default behaivour for Cabal, and the boolean
`forceParallelBuilding` will force a parallel build for faster development, at
the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).
