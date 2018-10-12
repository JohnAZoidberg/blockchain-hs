{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16-bytestring, bytestring
      , cryptonite, either, memory, stdenv, text, time
      }:
      mkDerivation {
        pname = "blockchain-johnazoidberg";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base16-bytestring bytestring cryptonite either memory text
          time
        ];
        executableHaskellDepends = [
          base base16-bytestring bytestring cryptonite either memory text
          time
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
