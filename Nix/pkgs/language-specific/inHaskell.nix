{ haskellPackages, lib }:
lib.genAttrs
  [
    "cliargs"
    "input"
    "id-generator"
  ]
  (
    packageName:
    haskellPackages.callPackage (
      { packageName }:
      let
        pname = packageName;
      in
      haskellPackages.mkDerivation {
        inherit pname;
        version = "0.1.0.0";
        src = ../../../Haskell/${pname};
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ haskellPackages.base ];
        description = "My Haskell Project - ${pname}";
        license = [ ];
        mainProgram = "${pname}";
      }
    ) { inherit packageName; }
  )
