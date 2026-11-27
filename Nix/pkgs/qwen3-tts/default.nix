{ nixpkgs, system }:
let
  pkgs = import nixpkgs {
    inherit system;
    config.rocmSupport = true;
  };
  # Keep Qwen's exact Python pins in one interpreter, including transitive users.
  python = pkgs.python312.override {
    packageOverrides = final: prev: {
      torch = prev.torch.override { gpuTargets = [ "gfx1030" ]; };
      torchaudio = prev.torchaudio.override { gpuTargets = [ "gfx1030" ]; };

      transformers = prev.transformers.overridePythonAttrs (_: rec {
        version = "4.57.3";
        src = pkgs.fetchPypi {
          pname = "transformers";
          inherit version;
          hash = "sha256-30lFApqt3XwJ7sXK2FHzBmL4vRdGchs0zAMdcMZa/rw=";
        };
      });
      accelerate = prev.accelerate.overridePythonAttrs (_: rec {
        version = "1.12.0";
        src = pkgs.fetchPypi {
          pname = "accelerate";
          inherit version;
          hash = "sha256-cJiMNS/rSBiHB30quEUSUCSyoTelCQ1tejK1fQOkXfY=";
        };
        # The upstream integration suite uses model downloads and multiple devices.
        doCheck = false;
      });
      pysox = final.callPackage ./pysox.nix { };
      qwen-tts = final.callPackage ./package.nix { soxCli = pkgs.sox; };
    };
  };
in
{
  inherit (python.pkgs) qwen-tts;

  shell = pkgs.mkShellNoCC {
    packages = [
      (python.withPackages (ps: [ ps.qwen-tts ]))
      pkgs.sox
      pkgs.rocmPackages.rocminfo
    ];
    # Navi 22 (gfx1031) uses the gfx1030 code objects in this environment.
    HSA_OVERRIDE_GFX_VERSION = "10.3.0";
  };
}
