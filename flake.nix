{
  description = "WSdlly02's Code Library";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    # Qwen3-TTS uses the Transformers 4 / tokenizers 0.22 stack.
    nixpkgs-tts.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs =
    inputs:
    let
      inherit (inputs.nixpkgs) lib;
      inherit (inputs.self.lib) pkgs';
      exposedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      # this function folds over all exposed systems and merges the results
      forExposedSystems = f: lib.foldl' lib.recursiveUpdate { } (map f exposedSystems);
    in
    {
      lib.pkgs' =
        {
          nixpkgsInstance ? inputs.nixpkgs,
          config ? { },
          overlays ? [ ],
          system,
        }:
        import nixpkgsInstance {
          inherit system;
          config = {
            allowAliases = false;
            allowUnfree = true;
          }
          // config;
          overlays = [ inputs.self.overlays.default ] ++ overlays;
        };
      overlays.default =
        final: _prev:
        import ./Nix/pkgs {
          pkgs = final;
        };
    }
    // forExposedSystems (
      system:
      let
        pkgs = pkgs' { inherit system; };
        tts = import ./Nix/pkgs/qwen3-tts {
          inherit system;
          nixpkgs = inputs.nixpkgs-tts;
        };
        exposedPackages = import ./Nix/pkgs { inherit pkgs; };
      in
      {
        packages."${system}" = lib.optionalAttrs (system == "x86_64-linux") { inherit (tts) qwen-tts; };
        devShells."${system}" = lib.optionalAttrs (system == "x86_64-linux") { qwen-tts = tts.shell; };
        formatter."${system}" = pkgs.nixfmt-tree;
        legacyPackages."${system}" = exposedPackages;
      }
    );
}
