{ pkgs }:
let
  python = pkgs.python312;
  qwen-tts = python.pkgs.callPackage ./package.nix {
    pysox = python.pkgs.callPackage ./pysox.nix { };
    soxCli = pkgs.sox;
  };
in
{
  inherit qwen-tts;

  shell = pkgs.mkShellNoCC {
    packages = [
      (python.withPackages (_: [ qwen-tts ]))
      pkgs.sox
      pkgs.rocmPackages.rocminfo
    ];
    # Navi 22 uses the gfx1030 code objects included by Nixpkgs.
    HSA_OVERRIDE_GFX_VERSION = "10.3.0";
  };
}
