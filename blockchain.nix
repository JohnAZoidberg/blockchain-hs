{ mkDerivation, base, cryptonite, stdenv, text }:
mkDerivation {
  pname = "blockchain";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cryptonite text ];
  license = stdenv.lib.licenses.bsd3;
}
