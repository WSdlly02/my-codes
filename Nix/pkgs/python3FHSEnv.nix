{
  buildFHSEnv,
  rocmPackages,
  symlinkJoin,
  writeShellScriptBin,
}:
let
  entrypoint = writeShellScriptBin "entrypoint" ''
    echo "[INFO] FHS Entrypoint: Setting up environment..."

    # 1. 动态查找 FHS 环境中的 site-packages 路径
    FHS_SITE_PACKAGES=$(ls -d /usr/lib/python*/site-packages)

    # 2. 关键：将系统包路径导出到 PYTHONPATH
    # 我们还保留了可能已存在的 PYTHONPATH,以支持更复杂的场景
    export PYTHONPATH="$FHS_SITE_PACKAGES''${PYTHONPATH:+:}$PYTHONPATH"

    # 3. 设置环境变量
    export ROCM_PATH=/usr
    export LD_LIBRARY_PATH=/usr/lib
    # export ROCM_PATH=${rocmtoolkit_joined}
    # export ROCM_SOURCE_DIR=${rocmtoolkit_joined}
    # export CMAKE_CXX_FLAGS="-I${rocmtoolkit_joined}/include -I${rocmtoolkit_joined}/include/rocblas"
    # export AOTRITON_INSTALLED_PREFIX = "${rocmPackages.aotriton}";
    source .venv/bin/activate
    echo "[INFO] Environment ready. PYTHONPATH is set. Handing over to fish shell..."

    # 4. 移交控制权给 fish shell
    fish
  '';
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
      rocm-smi
      clr.icd
      hipify
      # Optional
      # magma-hip
      # llvm.openmp
      # nccl
      # clr
    ];
    postBuild = ''
      rm -rf $out/nix-support
    '';
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
      (python3Env.override {
        extraPackages =
          f:
          with f;
          lib.optionals config.rocmSupport [
            torch
            torchsde
            torchvision
            torchaudio
            transformers
            tokenizers
            sentencepiece
            safetensors
          ];
      })
      stdenv.cc # gcc
      stdenv.cc.libc # glibc
      # stdenv.cc.cc -> gcc-unwrapped
      stdenv.cc.cc.lib # gcc-unwrapped-lib
      udev
      zlib
      zstd
    ]
    ++ lib.optionals config.rocmSupport [ rocmtoolkit_joined ];
  runScript = "${entrypoint}/bin/entrypoint";
}
