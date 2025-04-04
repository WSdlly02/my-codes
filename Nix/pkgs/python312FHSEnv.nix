{
  buildFHSEnv,
  dbus,
  fish,
  gcc,
  glib,
  glibc,
  inputs,
  libdrm,
  libglvnd,
  rocmPackages,
  stdenv,
  system,
  udev,
  zlib,
  zstd,
}:
let
  usedRocmPackages =
    if (system != "x86_64-linux") then
      [ ]
    else
      with rocmPackages;
      [
        clr
        clr.icd
        hipblas
        hipcub
        hipfft
        hipify
        hipsolver
        hipsparse
        llvm.openmp
        miopen
        #miopengemm
        rccl
        rocblas
        rocfft
        rocm-comgr
        rocm-core
        rocm-device-libs
        rocm-runtime
        rocm-smi
        rocm-thunk
        rocminfo
        rocprim
        rocrand
        rocsolver
        rocsparse
        rocthrust
        roctracer
      ];
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
      inputs.self.legacyPackages."${system}".python312Env
      libdrm
      libglvnd
      stdenv.cc.cc.lib
      udev
      zlib
      zstd
    ]
    ++ usedRocmPackages;
  profile = "export LD_LIBRARY_PATH=/usr/lib";
  runScript = "fish";
}
