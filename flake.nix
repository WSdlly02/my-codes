{
  description = "WSdlly02's Code Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
    }@inputs:
    let
      inherit (nixpkgs) lib;
      inherit (self) mkPkgs;
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

      formatter = forExposedSystems (system: (mkPkgs { inherit system; }).nixfmt-rfc-style);

      legacyPackages = forExposedSystems (
        system:
        with (mkPkgs { inherit system; });
        self.overlays.exposedPackages null (mkPkgs {
          inherit system;
        })
        // {
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
              cOneHundred =
                lib.genAttrs
                  (lib.forEach [
                    1
                    2
                    3
                    4
                    5
                    6
                    # 7 9 10 is skipped
                    8
                    11
                    12
                  ] (x: toString x))
                  (
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
      );

      mkPkgs =
        {
          config ? { },
          overlays ? [ ],
          system,
        }:
        import nixpkgs {
          inherit system;
          config = {
            allowAliases = false;
            allowUnfree = true;
            rocmSupport = true; # Notice !!!
            warnUndeclaredOptions = true;
          } // config;
          overlays = [
            self.overlays.exposedPackages
          ] ++ overlays;
        };

      overlays = {
        exposedPackages =
          final: prev: with prev; {
            haskellEnv = callPackage ./Nix/pkgs/haskellEnv.nix { };
            ncmdump = callPackage ./Nix/pkgs/ncmdump.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            python312Env = callPackage ./Nix/pkgs/python312Env.nix { inherit inputs; };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { };
          };
      };
    };
}
