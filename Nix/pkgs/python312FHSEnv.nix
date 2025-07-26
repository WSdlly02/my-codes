{
  buildFHSEnv,
  config,
  dbus,
  fish,
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
      composable_kernel
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
    ];
  };
in
buildFHSEnv {
  name = "python312FHSEnv";
  targetPkgs =
    pkgs:
    [
      # Common pkgs
      dbus
      fish
      libdrm
      libglvnd
      python312Env
      stdenv.cc # gcc
      stdenv.cc.libc # glibc
      # stdenv.cc.cc -> gcc-unwrapped
      stdenv.cc.cc.lib # gcc-unwrapped-lib
      udev
      zlib
      zstd
    ]
    ++ lib.optionals config.rocmSupport [ rocmtoolkit_joined ];
  profile = "export LD_LIBRARY_PATH=/usr/lib";
  runScript = "fish";
}
