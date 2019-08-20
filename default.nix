let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./blog.nix { }
