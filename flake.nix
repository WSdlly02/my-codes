{
  description = "WSdlly02's Code Library";

  inputs = {
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      flake-parts,
      self,
      nixpkgs,
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays = { };
      perSystem =
        {
          self',
          system,
          ...
        }:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
            };
            overlays = [ ];
          };
          inherit (pkgs)
            callPackage
            mkShell
            writeShellScriptBin
            ;
          inherit (pkgs.python312Packages)
            toPythonModule
            ;
        in
        {
          devShells = {
            default = callPackage ./Nix/devShells-default.nix { inherit inputs; };
          };

          formatter = pkgs.nixfmt-rfc-style;

          legacyPackages = {
            ####################
            haskellEnv = callPackage ./Nix/pkgs/haskellEnv.nix { };
            python312Env = callPackage ./Nix/pkgs/python312Env.nix { inherit inputs; };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { inherit inputs; }; # depends on python312Env
            # Drivers
            kmsxx-src = callPackage ./Nix/pkgs/kmsxx-src.nix { };
            libcamera = toPythonModule (callPackage ./Nix/pkgs/libcamera-raspi.nix { inherit inputs; });
            libpisp = callPackage ./Nix/pkgs/libpisp.nix { };
            picamera2 = callPackage ./Nix/pkgs/picamera2.nix { };
            pidng = callPackage ./Nix/pkgs/pidng.nix { };
            rpi-kms = toPythonModule (callPackage ./Nix/pkgs/rpi-kms.nix { inherit inputs; });
            simplejpeg = callPackage ./Nix/pkgs/simplejpeg.nix { };
            v4l2-python3 = callPackage ./Nix/pkgs/v4l2-python3.nix { };
            mlx90460-driver =
              let
                driverPath = "./Nix/pkgs/mlx90460-driver";
              in
              {
                Adafruit-Blinka = callPackage ./${driverPath}/Adafruit-Blinka.nix { };
                adafruit-circuitpython-busdevice =
                  callPackage ./${driverPath}/adafruit-circuitpython-busdevice.nix
                    { };
                adafruit-circuitpython-connectionmanager =
                  callPackage ./${driverPath}/adafruit-circuitpython-connectionmanager.nix
                    { };
                adafruit-circuitpython-mlx90640 =
                  callPackage ./${driverPath}/adafruit-circuitpython-mlx90640.nix
                    { };
                adafruit-circuitpython-requests =
                  callPackage ./${driverPath}/adafruit-circuitpython-requests.nix
                    { };
                adafruit-circuitpython-typing = callPackage ./${driverPath}/adafruit-circuitpython-typing.nix { };
                rpi-ws281x = callPackage ./${driverPath}/rpi-ws281x.nix { };
              };
            ####################
            inC =
              nixpkgs.lib.genAttrs
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
                  nixpkgs.lib.genAttrs
                    (nixpkgs.lib.forEach [
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
            inHaskell = nixpkgs.lib.genAttrs [
              "cliargs"
              "input"
            ] (packageName: callPackage ./Haskell { pname = packageName; });
            inPython =
              nixpkgs.lib.genAttrs
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
                    # ${self'.legacyPackages.python312Env}/bin/python3.12 ./Python/${packageName}.py $@
                    python3.12 ./Python/${packageName}.py $@
                  ''
                );
            inRust =
              nixpkgs.lib.genAttrs
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
          };
        };
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
    };
}
