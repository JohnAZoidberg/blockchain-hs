{ mkDerivation, base, bytestring, cryptonite, either, memory
, stdenv, text, time
}:
mkDerivation {
  pname = "blockchain";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cryptonite either memory text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
