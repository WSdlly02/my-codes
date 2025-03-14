{
  description = "WSdlly02's Codes Library";

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
      perSystem =
        {
          inputs',
          self',
          ...
        }:
        let
          pkgs = inputs'.nixpkgs.legacyPackages; # can be defined in arguments
          inherit (pkgs)
            callPackage
            mkShell
            writeShellScriptBin
            ;
        in
        {
          devShells = {
            default = mkShell {
              packages = with pkgs; [
                (haskellPackages.ghcWithPackages (
                  pkgs: with pkgs; [
                    cabal-install
                    stack
                  ]
                ))
                (self'.legacyPackages.python312Env.override {
                  extraPackages =
                    with pkgs.python312Packages;
                    with self'.legacyPackages;
                    [
                      # Drivers from self
                      mlx90460-driver.Adafruit-Blinka
                      mlx90460-driver.adafruit-circuitpython-busdevice
                      mlx90460-driver.adafruit-circuitpython-connectionmanager
                      mlx90460-driver.adafruit-circuitpython-mlx90640
                      mlx90460-driver.adafruit-circuitpython-requests
                      mlx90460-driver.adafruit-circuitpython-typing
                      mlx90460-driver.rpi-ws281x
                      picamera2
                      v4l2
                      # Drivers from Nixpkgs
                      adafruit-platformdetect
                      adafruit-pureio
                      binho-host-adapter
                      pyftdi
                      pyserial
                      pyusb
                      rpi-gpio
                      sysv-ipc
                      typing-extensions
                      pillow
                      # Daily runtimes
                      flask
                      icalendar # For generating calendar files
                      ##opencv4
                    ];
                })
              ];
              shellHook = ''
                fish
              '';
            };
          };

          formatter = pkgs.nixfmt-rfc-style;

          legacyPackages = {
            ####################
            python312Env = callPackage ./Nix/pkgs/python312Env.nix { };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { inherit inputs; }; # depends on python312Env
            picamera2 = callPackage ./Nix/pkgs/picamera2.nix { };
            v4l2 = callPackage ./Nix/pkgs/v4l2.nix { };
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
