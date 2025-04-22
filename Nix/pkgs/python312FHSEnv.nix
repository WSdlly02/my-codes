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
  symlinkJoin,
  udev,
  zlib,
  zstd,
}:
let
  rocmtoolkit_joined = symlinkJoin {
    name = "rocm-merged";
    paths = with rocmPackages; [
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
      clr.icd
      hipify
      rocm-smi
    ];
  };
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
    ++ lib.optionals config.rocmSupport [ rocmtoolkit_joined ];
  profile = "export LD_LIBRARY_PATH=/usr/lib";
  runScript = "fish";
}
