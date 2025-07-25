{
  mkDerivation,
  base,
  bytestring,
  crypton,
  directory,
  lib,
  process,
}:
mkDerivation {
  pname = "id-generator";
  version = "1.1.0.0";
  src = ../../Haskell/id-generator;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    bytestring
    crypton
    directory
    process
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "id-generator";
}
