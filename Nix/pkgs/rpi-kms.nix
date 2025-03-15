{
  fetchPypi,
  fmt,
  git,
  inputs,
  kmsxx,
  lib,
  libdrm,
  meson,
  ninja,
  pkg-config,
  python312Packages,
  stdenv,
  system,
}:
stdenv.mkDerivation rec {
  pname = "rpi-kms";
  version = "0.1a1";
  src = fetchPypi rec {
    pname = "rpi_kms";
    inherit version;
    hash = "sha256-+TKLjTgCznrGO9KFsBX0cWD6G2SxZDufeTUz5EONB/g=";
  };
  patchPhase = ''
    patchShebangs clone-kmsxx.sh
    sed -i 's|\bgit\b|git -c safe.directory="${
      inputs.self.legacyPackages."${system}".kmsxx-src
    }/.git"|g' clone-kmsxx.sh
    cat >>meson_options.txt<< EOF
    option('libutils', type : 'boolean', value : true,
        description : 'Build kms++utils library')
    option('utils', type : 'boolean', value : true,
        description : 'Build an assortment of kms++ utils and tests')
    option('pykms', type : 'feature', value : 'auto',
        description : 'Build python bindings')
    option('kmscube', type : 'boolean', value : false,
        description : 'Build kmscube test application')
    EOF
  '';
  mesonFlags = [
    "-Drepository=${inputs.self.legacyPackages."${system}".kmsxx-src}"
    "-Drevision=master"
  ];
  nativeBuildInputs = [
    git
    meson
    ninja
  ];
  buildInputs = [
    fmt
    kmsxx
    libdrm
    pkg-config
    python312Packages.pybind11
  ];

  meta = with lib; {
    homepage = "https://sourceforge.net/p/raspberry-gpio-python";
    description = "Python module to control the GPIO on a Raspberry Pi";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ onny ];
  };
}
