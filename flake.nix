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
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      pkgs =
        system:
        nixpkgs.legacyPackages."${system}".appendOverlays [
          (final: prev: {
            config = prev.config // {
              allowUnfree = true;
              allowUnsupportedSystem = true;
              enableParallelBuilding = true;
              rocmSupport = true;
            };
          })
          self.overlays.legacyPackagesExposed
        ];
    in
    {
      overlays = {
        legacyPackagesExposed =
          final: prev: with prev; {
            haskellEnv = callPackage ./Nix/pkgs/haskellEnv.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            python312Env = callPackage ./Nix/pkgs/python312Env.nix { inherit inputs; };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { };
          };
      };

      devShells = forAllSystems (
        system: with (pkgs system); {
          default = callPackage ./Nix/devShells-default.nix { inherit inputs; };
        }
      );

      formatter = forAllSystems (system: (pkgs system).nixfmt-rfc-style);

      legacyPackages = forAllSystems (
        system: with (pkgs system); {
          ####################
          haskellEnv = callPackage ./Nix/pkgs/haskellEnv.nix { };
          ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
          python312Env = callPackage ./Nix/pkgs/python312Env.nix { inherit inputs; };
          python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { }; # depends on python312Env
          ####################
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
    };
}
