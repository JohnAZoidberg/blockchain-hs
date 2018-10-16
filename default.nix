with import <nixpkgs> {};
let
  drv = haskellPackages.callCabal2nix "blockchain-johnazoidberg" (lib.cleanSource ./.) {};
in
  if pkgs.lib.inNixShell then drv.env else drv
