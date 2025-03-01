{
  haskellPackages,
  pname,
}:
haskellPackages.mkDerivation {
  inherit pname;
  version = "0.1.0.0";
  src = ./${pname}.hs;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ haskellPackages.base ];
  description = "My Haskell Project - ${pname}";
  license = [ ];
  mainProgram = "${pname}";
}
