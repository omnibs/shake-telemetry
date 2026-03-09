{ pkgs, ... }:

{
  packages = [
    pkgs.cabal-install
    (pkgs.haskell.packages.ghc98.ghcWithPackages (hp:
      with hp; [
        aeson
        aeson-pretty
        bytestring
        clock
        containers
        directory
        filepath
        hashable
        hedgehog
        process
        shake
        stm
        tasty
        tasty-hedgehog
        tasty-hunit
        temporary
        text
        time
        unordered-containers
        vector
      ]))
  ];
}
