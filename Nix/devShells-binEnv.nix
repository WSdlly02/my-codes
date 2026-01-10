{
  lib,
  mkShell,
  pkgs,
  symlinkJoin,
  enableRocmSupport ? false,
}:
let
  vendorComposableKernel = !pkgs.rocmPackages.composable_kernel.anyMfmaTarget;
  rocmtoolkit_joined = symlinkJoin {
    name = "rocm-merged";
    paths =
      with pkgs.rocmPackages;
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
        # Optional
        # magma-hip
        # llvm.openmp
        # nccl
        # clr
      ]
      ++ lib.optionals (!vendorComposableKernel) [
        composable_kernel
      ];
    postBuild = ''
      rm -rf $out/nix-support
    '';
  };
in
mkShell rec {
  packages =
    with pkgs;
    [
      dbus
      glibcLocales
      glibc
      gcc.cc.lib
      coreutils
      less
      shadow
      su
      gawk
      diffutils
      findutils
      gnused
      gnugrep
      gnutar
      gzip
      bzip2
      xz
      udev
      zlib
      zstd
      # pkgs here is runtime pkgs
      ffmpeg
      go
      nodejs
      npm-check-updates
    ]
    ++ lib.optionals enableRocmSupport [
      rocmtoolkit_joined
    ];
  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath packages}:$LD_LIBRARY_PATH
  '';
}
