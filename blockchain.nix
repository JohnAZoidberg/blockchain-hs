{ mkDerivation, base, base16-bytestring, bytestring, cryptonite
, either, memory, stdenv, text, time
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
}
