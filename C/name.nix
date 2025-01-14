{
  stdenv,
  libcs50,
}:
stdenv.mkDerivation {
  pname = "name";
  version = "0.0.1";
  src = ./name.c;
  dontUnpack = true;
  buildInputs = [
    libcs50
  ];
  buildPhase = ''
    mkdir -p $out/bin
    gcc $src -o $out/bin/name -lcs50
  '';
  doCheck = false;
}
