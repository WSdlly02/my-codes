{
  stdenv,
  # libcs50,
}:
stdenv.mkDerivation {
  pname = "loops";
  version = "0.0.1";
  src = ./loops.c;
  dontUnpack = true;
  buildInputs = [
    # libcs50
  ];
  buildPhase = ''
    mkdir -p $out/bin
    gcc $src -o $out/bin/loops
  '';
  doCheck = false;
}
