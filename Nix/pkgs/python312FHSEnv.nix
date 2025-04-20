{
  buildFHSEnv,
  config,
  dbus,
  fish,
  gcc,
  glib,
  glibc,
  lib,
  libdrm,
  libglvnd,
  rocmPackages,
  python312Env,
  stdenv,
  system,
  udev,
  zlib,
  zstd,
}:
let
  usedRocmPackages = lib.optionals config.rocmSupport (
    with rocmPackages;
    [
      rocm-core
      clr
      rccl
      miopen
      aotriton
      rocrand
      rocblas
      rocsparse
      hipsparse
      rocthrust
      rocprim
      hipcub
      roctracer
      rocfft
      rocsolver
      hipfft
      hiprand
      hipsolver
      hipblas-common
      hipblas
      hipblaslt
      rocminfo
      rocm-comgr
      rocm-device-libs
      rocm-runtime
      rocm-smi
      clr.icd
      hipify
    ]
  );
in
buildFHSEnv {
  name = "python312FHSEnv";
  targetPkgs =
    pkgs:
    with pkgs;
    [
      # Common pkgs
      dbus
      fish
      gcc
      glib.out
      glibc
      libdrm
      libglvnd
      python312Env
      stdenv.cc.cc.lib
      udev
      zlib
      zstd
    ]
    ++ usedRocmPackages;
  profile = "export LD_LIBRARY_PATH=/usr/lib";
  runScript = "fish";
}
