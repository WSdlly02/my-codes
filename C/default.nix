{
  lib,
  packageName,
  pname,
  stdenv,
}:
stdenv.mkDerivation {
  inherit pname;
  version = "0.0.1";
  src =
    if (lib.hasPrefix "cOneHundred" pname) then
      ./src/cOneHundred/${packageName}.c
    else
      ./src/${pname}.c;
  dontUnpack = true;
  preferLocalBuild = true;
  allowSubstitutes = false;
  buildInputs = [
  ];
  buildPhase = ''
    $CC $src -o ${pname}
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp ${pname} $out/bin
  '';
  doCheck = false;
}
