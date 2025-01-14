{
  stdenv,
  libcs50,
}:
stdenv.mkDerivation {
  pname = "agree";
  version = "0.0.1";
  src = ./agree.c;
  dontUnpack = true;
  buildInputs = [
    libcs50
  ];
  buildPhase = ''
    mkdir -p $out/bin
    gcc $src -o $out/bin/agree -lcs50
  '';
  doCheck = false;
}
