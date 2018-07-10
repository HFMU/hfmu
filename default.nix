{ mkDerivation, stdenv }:
mkDerivation {
  pname = "hfmu";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = false;
  license = stdenv.lib.licenses.bsd3;
}
