{
  doxygen,
  fetchFromGitHub,
  graphviz,
  gst_all_1,
  gtest,
  inputs,
  lib,
  libdrm,
  libevent,
  libyaml,
  lttng-ust,
  makeFontsConf,
  meson,
  ninja,
  openssl,
  pkg-config,
  python312,
  python312Packages,
  stdenv,
  system,
  systemd, # for libudev
}:

stdenv.mkDerivation rec {
  pname = "libcamera";
  version = "0.3.1+rpt20240906";

  src = fetchFromGitHub rec {
    owner = "raspberrypi";
    repo = "libcamera";
    rev = "v${version}";
    hash = "sha256-KH30jmHfxXq4j2CL7kv18DYECJRp9ECuWNPnqPZajPA=";
  };

  patches = [
    ./libcamera-installed.patch
    ./libcamera-no-timeout.patch
  ];

  outputs = [
    "out"
    "dev"
  ];

  postPatch = ''
    patchShebangs utils/
    patchShebangs src/py/libcamera
  '';

  # libcamera signs the IPA module libraries at install time, but they are then
  # modified by stripping and RPATH fixup. Therefore, we need to generate the
  # signatures again ourselves. For reproducibility, we use a static private key.
  #
  # If this is not done, libcamera will still try to load them, but it will
  # isolate them in separate processes, which can cause crashes for IPA modules
  # that are not designed for this (notably ipa_rpi.so).
  preBuild = ''
    ninja src/ipa-priv-key.pem
    install -D ${./libcamera-raspi-ipa-priv-key.pem} src/ipa-priv-key.pem
  '';

  strictDeps = true;

  buildInputs = [
    # IPA and signing
    openssl

    # gstreamer integration
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base

    # cam integration
    libevent
    libdrm

    # hotplugging
    systemd

    # lttng tracing
    lttng-ust

    # yamlparser
    libyaml

    gtest
    inputs.self.legacyPackages."${system}".libpisp
  ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    python312
    python312Packages.jinja2
    python312Packages.pyyaml
    python312Packages.ply
    python312Packages.sphinx
    python312Packages.pybind11
    graphviz
    doxygen
    openssl
  ];

  mesonFlags = [
    "-Dv4l2=true"
    "-Dqcam=disabled"
    "-Dlc-compliance=disabled" # tries unconditionally to download gtest when enabled
    # Avoid blanket -Werror to evade build failures on less
    # tested compilers.
    "-Dwerror=false"
    # Documentation breaks binary compatibility.
    # Given that upstream also provides public documentation,
    # we can disable it here.
    "-Ddocumentation=disabled"
    "-Dpipelines=rpi/vc4,rpi/pisp"
    "-Dipas=rpi/vc4,rpi/pisp"
  ];

  # Fixes error on a deprecated declaration
  env.NIX_CFLAGS_COMPILE = "-Wno-error=deprecated-declarations";

  # Silence fontconfig warnings about missing config
  FONTCONFIG_FILE = makeFontsConf { fontDirectories = [ ]; };

  meta = with lib; {
    description = "An open source camera stack and framework for Linux, Android, and ChromeOS";
    homepage = "https://libcamera.org";
  };
}
