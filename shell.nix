{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
pkgs.mkShell {
  buildInputs = [ pkgs.haskell.compiler.${compiler}
                  pkgs.haskell.packages.${compiler}.apply-refact
                  pkgs.haskell.packages.${compiler}.hlint
                  pkgs.haskell.packages.${compiler}.hindent
                  pkgs.haskell.packages.${compiler}.hoogle
                  pkgs.haskell.packages.${compiler}.hasktags
                  pkgs.haskell.packages.${compiler}.stylish-haskell
                  pkgs.haskell.packages.${compiler}.cabal2nix
                  pkgs.haskell.packages.${compiler}.cabal-install
                  pkgs.haskell.packages.${compiler}.ghcid
                  pkgs.haskell.packages.${compiler}.hspec-discover
                  (pkgs.haskell.packages.${compiler}.callPackage ./default.nix {})
                ];
}
