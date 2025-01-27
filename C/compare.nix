{
  stdenv,
  libcs50,
}:
stdenv.mkDerivation rec {
  pname = "compare";
  version = "0.0.1";
  src = ./${pname}.c;
  dontUnpack = true;
  preferLocalBuild = true;
  allowSubstitutes = false;
  buildInputs = [
    libcs50
  ];
  buildPhase = ''
    $CC $src -lcs50 -o ${pname}
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp ${pname} $out/bin
  '';
  doCheck = false;
}
