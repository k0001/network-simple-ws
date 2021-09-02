{ nixpkgs ? import ./nixpkgs.nix }:

let
pkgs = import nixpkgs {};
ghc8104 = pkgs.haskell.packages.ghc8104.override {
  packageSetConfig = import ./hs-overlay.nix { inherit pkgs; };
};

in { inherit (ghc8104) network-simple-ws _shell; }
