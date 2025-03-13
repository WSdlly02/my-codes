{
  buildFHSEnv,
  cmake,
  gcc,
  glibc,
  dbus,
  fish,
  libcamera,
  libdrm,
  ninja,
  ncurses,
  inputs,
  udev,
  system,
  zstd,
# numpy, # which is used in buildPythonPackages {propagatedBuildInputs=[];}
}:
let
  usedRocmPackages = [
    /*
      clr
      clr.icd
      # hipblas
      hip-common
      # rocblas
      rocm-runtime
      rocminfo
      rocm-smi
      # rocm-thunk
      rocm-comgr
      rocm-device-libs
    */
  ]; # A list
in
buildFHSEnv {
  name = "python312FHSEnv";
  targetPkgs =
    pkgs:
    with pkgs;
    [
      # Common pkgs
      cmake
      gcc
      glibc
      dbus
      fish
      libdrm
      libcamera
      ninja
      ncurses
      inputs.self.legacyPackages."${system}".python312Env
      udev
      zstd
    ]
    ++ usedRocmPackages;
  runScript = "fish";
}
