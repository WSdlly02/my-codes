{
  description = "WSdlly02's Code Library";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
  };

  outputs =
    inputs:
    let
      inherit (inputs.nixpkgs) lib;
      inherit (inputs.self.lib) mkPkgs;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forExposedSystems = lib.genAttrs systems;
    in
    {
      devShells = forExposedSystems (
        system: with (mkPkgs { inherit system; }); {
          default = callPackage ./Nix/devShells-default.nix { };
        }
      );

      formatter = forExposedSystems (system: (mkPkgs { inherit system; }).nixfmt);

      legacyPackages = forExposedSystems (
        system:
        with (mkPkgs { inherit system; });
        {
          # Packages here won't be exposed and used as a library
          inC = callPackage ./Nix/pkgs/language-specific/inC.nix { };
          inHaskell = callPackage ./Nix/pkgs/language-specific/inHaskell.nix { };
          inPython = callPackage ./Nix/pkgs/language-specific/inPython.nix { };
          inRust = callPackage ./Nix/pkgs/language-specific/inRust.nix { };
        }
        // inputs.self.overlays.exposedPackages null (mkPkgs {
          inherit system;
        })
        // inputs.self.overlays.extraPackages null (mkPkgs {
          inherit system;
        })
      );

      lib.mkPkgs =
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
            (final: prev: { path = "${nixpkgsInstance}"; })
          ]
          ++ overlays;
        };

      overlays = {
        exposedPackages =
          final: prev: with prev; {
            # Packages here will be exposed to nix-config and used as library
            audio-relay = callPackage ./Nix/pkgs/audio-relay.nix { };
            ncmdump = callPackage ./Nix/pkgs/ncmdump.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            # Haskell packages
            id-generator = haskellPackages.callPackage ./Nix/pkgs/id-generator.nix { };
            haskellEnv = haskellPackages.callPackage ./Nix/pkgs/haskellEnv.nix { };
            # Python packages
            pystun3 = python3Packages.callPackage ./Nix/pkgs/pystun3.nix { };
            python3Env = callPackage ./Nix/pkgs/python3Env.nix { };
            python3FHSEnv = callPackage ./Nix/pkgs/python3FHSEnv.nix { };
          };
        extraPackages = final: prev: {
          # Packages here will be used as library but won't be exposed

          /*
            python3 = prev.python3.override {
              packageOverrides = pyfinal: pyprev: {
                torch = pyprev.torch.override {
                  rocmSupport = true;
                  vulkanSupport = true;
                };
              };
            };
          */
        };
      };
    };
}
