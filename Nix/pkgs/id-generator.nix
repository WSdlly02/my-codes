{
  mkDerivation,
  base,
  lib,
  process,
}:
mkDerivation {
  pname = "id-generator";
  version = "0.1.1.1";
  src = ../../Haskell/id-generator;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    process
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "id-generator";
}
