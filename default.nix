{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall hakyll highlightingKate;
in cabal.mkDerivation (self: {
  pname = "haskell-id-website";
  version = "0.0.0.0";
  src = ./.;
  buildDepends = [
    hakyll highlightingKate
  ];
  buildTools = [ cabalInstall ];
})

