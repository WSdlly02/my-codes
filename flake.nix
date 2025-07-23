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
          comfyui = callPackage ./Nix/devShells-comfyui.nix { };
          default = callPackage ./Nix/devShells-default.nix { inherit inputs; };
        }
      );

      formatter = forExposedSystems (system: (mkPkgs { inherit system; }).nixfmt);

      legacyPackages = forExposedSystems (
        system:
        with (mkPkgs { inherit system; });
        {
          inC =
            lib.genAttrs
              [
                "array"
                "boolean"
                "discount"
                ##"duplicated-file-searcher"
                "fibonacci"
                "float"
                "for"
                "logical"
                "loops"
                "math"
                "pi"
                "pointer"
                "project-routine-scheduler"
                "readcsv"
                "scanf"
                "string"
                "switch"
                "test"
                "triangle"
                "var"
                "while"
              ]
              (
                packageName:
                callPackage ./C {
                  inherit packageName;
                  pname = packageName;
                }
              )
            // {
              cOneHundred = lib.genAttrs (map (x: toString x) (lib.range 1 12)) (
                packageName:
                callPackage ./C {
                  inherit packageName;
                  pname = "cOneHundred-" + "${packageName}";
                }
              );
            };
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
                # pkgs.runCommandLocal just running commands without leaving any executable files
                # But nix run .# requires a binary to execute
                # It's just suitable for installing some misc files
                # Shebang will inherit env vars
                # But cannot export $PATH vars
                writeShellScriptBin "${packageName}-wrapper" ''
                  python3.12 ./Python/${packageName}.py $@
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
            haskellEnv = callPackage ./Nix/pkgs/haskellEnv.nix { };
            ncmdump = callPackage ./Nix/pkgs/ncmdump.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            python312Env = callPackage ./Nix/pkgs/python312Env.nix { inherit inputs; };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { };
            id-generator = haskellPackages.callPackage ./Nix/pkgs/id-generator.nix { };
          };
      };
    };
}
