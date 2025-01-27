{
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "float";
  version = "0.0.1";
  src = ./${pname}.c;
  dontUnpack = true;
  preferLocalBuild = true;
  allowSubstitutes = false;
  buildInputs = [
    # libcs50
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
