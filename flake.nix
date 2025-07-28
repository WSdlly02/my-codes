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
          default = callPackage ./Nix/devShells-default.nix { inherit inputs; };
        }
      );

      formatter = forExposedSystems (system: (mkPkgs { inherit system; }).nixfmt);

      legacyPackages = forExposedSystems (
        system:
        with (mkPkgs { inherit system; });
        {
          inC = import ./Nix/pkgs/inC.nix { inherit callPackage lib; };
          inHaskell = lib.genAttrs [
            "cliargs"
            "input"
          ] (packageName: callPackage ./Haskell { pname = packageName; });
          inPython =
            lib.genAttrs
              [
                "class-schedule"
                "duplicated-file-searcher"
                "fibonacci"
                "project-routine-scheduler"
                "roots-resolver"
                "teaching-week-reminder"
              ]
              (
                packageName:
                writeShellScriptBin "${packageName}-wrapper" ''
                  python3 ./Python/${packageName}.py $@
                ''
              );
          inRust =
            lib.genAttrs
              [
                "fibonacci"
                "guessing-game"
                "hello-world"
                "temperature-converter"
              ]
              (
                packageName:
                callPackage ./Rust {
                  pname = packageName;
                }
              );
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
            (final: prev: { path = "${nixpkgsInstance}"; })
          ]
          ++ overlays;
        };

      overlays = {
        exposedPackages =
          final: prev: with prev; {
            audio-relay = callPackage ./Nix/pkgs/audio-relay.nix { };
            ncmdump = callPackage ./Nix/pkgs/ncmdump.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            # Haskell packages
            id-generator = haskellPackages.callPackage ./Nix/pkgs/id-generator.nix { };
            haskellEnv = haskellPackages.callPackage ./Nix/pkgs/haskellEnv.nix { };
            # Python packages
            pystun3 = python3Packages.callPackage ./Nix/pkgs/pystun3.nix { };
            python3Env = python3Packages.callPackage ./Nix/pkgs/python3Env.nix { };
            python3FHSEnv = callPackage ./Nix/pkgs/python3FHSEnv.nix { };
          };
        extraPackages = final: prev: { };
      };
    };
}
