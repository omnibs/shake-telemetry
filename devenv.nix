{ pkgs, ... }:

{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc98;
    lsp.enable = false;
    cabal.enable = true;
  };

  packages = [
    pkgs.cabal-install
  ];
}
