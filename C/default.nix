{
  cs50 ? false,
  lib,
  libcs50,
  pname,
  stdenv,
}:
stdenv.mkDerivation {
  inherit pname;
  version = "0.0.1";
  src = ./src/${pname}.c;
  dontUnpack = true;
  preferLocalBuild = true;
  allowSubstitutes = false;
  buildInputs = [
  ] ++ lib.optionals cs50 [ libcs50 ];
  buildPhase = ''
    $CC $src ${lib.optionalString cs50 "-lcs50"} -o ${pname}
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp ${pname} $out/bin
  '';
  doCheck = false;
}
