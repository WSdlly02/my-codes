{
  cmake,
  fetchFromGitHub,
  lib,
  meson,
  ninja,
  nlohmann_json,
  pkg-config,
  stdenv,
}:

stdenv.mkDerivation rec {
  pname = "libpisp";
  version = "1.0.7";

  src = fetchFromGitHub {
    owner = "raspberrypi";
    repo = "libpisp";
    rev = "v${version}";
    hash = "sha256-Fo2UJmQHS855YSSKKmGrsQnJzXog1cdpkIOO72yYAM4=";
  };

  strictDeps = true;

  buildInputs = [
    nlohmann_json
  ];

  nativeBuildInputs = [
    meson
    cmake
    pkg-config
    ninja
  ];

  mesonFlags = [
    "-Dlogging=disabled"
    #   "-Dv4l2=true"
    #   "-Dqcam=${if withQcam then "enabled" else "disabled"}"
    #   "-Dlc-compliance=disabled" # tries unconditionally to download gtest when enabled
    #   # Avoid blanket -Werror to evade build failures on less
    #   # tested compilers.
    #   "-Dwerror=false"
    #   # Documentation breaks binary compatibility.
    #   # Given that upstream also provides public documentation,
    #   # we can disable it here.
    #   "-Ddocumentation=disabled"
  ];

  # Fixes error on a deprecated declaration
  env.NIX_CFLAGS_COMPILE = "-Wno-error=deprecated-declarations";
  postInstall = ''
    mkdir -p $out/lib/libpisp/backend/
    cp $out/share/libpisp/backend_default_config.json $out/lib/libpisp/backend/
  '';
}
