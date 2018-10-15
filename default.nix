with import <nixpkgs> {};
let
  drv = haskellPackages.callPackage ./blockchain.nix {};
in
  if pkgs.lib.inNixShell then drv.env else drv
