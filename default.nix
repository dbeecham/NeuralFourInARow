{ mkDerivation, base, hmatrix, mtl, random, stdenv }:
mkDerivation {
  pname = "NeuralFourInARow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hmatrix mtl random ];
  license = stdenv.lib.licenses.mit;
}
