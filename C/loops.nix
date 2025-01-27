{
  stdenv,
# libcs50,
}:
stdenv.mkDerivation rec {
  pname = "loops";
  version = "0.0.1";
  src = ./${pname}.c;
  dontUnpack = true;
  preferLocalBuild = true;
  allowSubstitutes = false;
  buildInputs = [
    # libcs50
  ];
  buildPhase = ''
    mkdir -p $out/bin
    $CC $src -o $out/bin/${pname}
  '';
  doCheck = false;
}
