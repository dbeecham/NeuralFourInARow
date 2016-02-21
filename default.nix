{ mkDerivation, base, base-prelude, hmatrix, MonadRandom, mtl
, random, stdenv
}:
mkDerivation {
  pname = "NeuralFourInARow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base-prelude hmatrix MonadRandom mtl random
  ];
  license = stdenv.lib.licenses.mit;
}
