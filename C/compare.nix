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
    mkdir -p $out/bin
    $CC $src -o $out/bin/${pname} -lcs50
  '';
  doCheck = false;
}
