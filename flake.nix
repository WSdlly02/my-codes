{
  description = "WSdlly02's Code Library";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
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
      forExposedSystems = f: builtins.foldl' lib.recursiveUpdate { } (map f exposedSystems);
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
            rocmSupport = true; # Notice !!!
          }
          // config;
          overlays = [
            inputs.self.overlays.exposedPackages
            inputs.self.overlays.extraPackages
          ]
          ++ overlays;
        };
      overlays = {
        exposedPackages =
          final: prev: inputs.self.legacyPackages."${prev.stdenv.hostPlatform.system}".exposedPackages;
        # Packages here will be exposed to nix-config and used as library

        extraPackages =
          final: prev: inputs.self.legacyPackages."${prev.stdenv.hostPlatform.system}".extraPackages;
        # Packages here will be used as library but won't be exposed
        /*
          {
            python3 = prev.python3.override {
              packageOverrides = pyfinal: pyprev: {
                torch = pyprev.torch.override {
                  rocmSupport = true;
                  vulkanSupport = true;
                };
              };
            };
          }
        */
      };
    }
    // forExposedSystems (
      system: with (pkgs' { inherit system; }); {
        devShells."${system}" = {
          default = callPackage ./Nix/devShells-default.nix { };
        };
        formatter."${system}" = nixfmt-tree;
        legacyPackages."${system}" = {
          # Packages here won't be exposed and used as a library
          inC = callPackage ./Nix/pkgs/language-specific/inC.nix { };
          inHaskell = callPackage ./Nix/pkgs/language-specific/inHaskell.nix { };
          inPython = callPackage ./Nix/pkgs/language-specific/inPython.nix { };
          inRust = callPackage ./Nix/pkgs/language-specific/inRust.nix { };
          exposedPackages = {
            audio-relay = callPackage ./Nix/pkgs/audio-relay.nix { };
            ncmdump = callPackage ./Nix/pkgs/ncmdump.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            qoder = callPackage ./Nix/pkgs/qoder.nix { };
            # Haskell packages
            id-generator = haskellPackages.callPackage ./Nix/pkgs/id-generator.nix { };
            haskellEnv = haskellPackages.callPackage ./Nix/pkgs/haskellEnv.nix { };
            # Python packages
            pystun3 = python3Packages.callPackage ./Nix/pkgs/pystun3.nix { };
            python3Env = python3Packages.callPackage ./Nix/pkgs/python3Env.nix { };
            python3FHSEnv = callPackage ./Nix/pkgs/python3FHSEnv.nix { };
          };
          extraPackages = { };
        };
      }
    );
}
