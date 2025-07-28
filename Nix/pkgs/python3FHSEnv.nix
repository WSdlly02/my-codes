{
  buildFHSEnv,
  rocmPackages,
  symlinkJoin,
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
  name = "python3FHSEnv";
  targetPkgs =
    f:
    with f;
    [
      # Common pkgs
      dbus
      fish
      libdrm
      libglvnd
      python3Env
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
