{
  stdenv,
  libcs50,
}:
stdenv.mkDerivation {
  pname = "compare";
  version = "0.0.1";
  src = ./compare.c;
  dontUnpack = true;
  buildInputs = [
    libcs50
  ];
  buildPhase = ''
    mkdir -p $out/bin
    gcc $src -o $out/bin/compare -lcs50
  '';
  doCheck = false;
}
