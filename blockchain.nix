{ mkDerivation, base, base16-bytestring, bytestring, cryptonite
, either, memory, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, text, time
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
  executableHaskellDepends = [ base text time ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-hunit tasty-quickcheck tasty-smallcheck
  ];
  license = stdenv.lib.licenses.bsd3;
}
