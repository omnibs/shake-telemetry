{ pkgs, ... }:

{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc98;
  };

  packages = [
    pkgs.cabal-install
  ];
}
