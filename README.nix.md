Nix expressions are provided to ease building Qtah within
[Nixpkgs](https://nixos.org/nixpkgs).  Qtah can be inserted into Nixpkgs with
the following overrides, after updating `qtahDir` as appropriate for your
environment.

    packageOverrides = pkgs:
      let qtahDir = /my/projects/qtah.git;
          qt = pkgs.qt5.qtbase;  # (Or pkgs.qt4.)
      in rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            qtah-generator = self.callPackage (qtahDir + /qtah-generator) {};
            qtah-cpp = self.callPackage (qtahDir + /qtah-cpp) { inherit qt; };
            qtah = self.callPackage (qtahDir + /qtah) { inherit qt; };
            qtah-examples = self.callPackage (qtahDir + /qtah-examples) {};
          };
        };
      };

The Haskell packages accept two optional parameters.  `enableSplitObjs`, when
non-null, will override Nixpkgs's default behaivour for Cabal, and the boolean
`forceParallelBuilding` will force a parallel build for faster development, at
the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).

---

This file is part of Qtah.

Copyright 2015-2018 The Qtah Authors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
