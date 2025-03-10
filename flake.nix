{
  description = "WSdlly02's Codes Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-parts,
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
                (self'.legacyPackages.python312Env.override {
                  extraPackages = with pkgs.python312Packages; [
                    self'.legacyPackages.mlx90460-driver.adafruit-circuitpython-mlx90640
                    self'.legacyPackages.mlx90460-driver.Adafruit-Blinka
                    self'.legacyPackages.mlx90460-driver.adafruit-circuitpython-typing
                    self'.legacyPackages.mlx90460-driver.adafruit-circuitpython-busdevice
                    self'.legacyPackages.mlx90460-driver.adafruit-circuitpython-requests
                    self'.legacyPackages.mlx90460-driver.adafruit-circuitpython-connectionmanager
                    self'.legacyPackages.mlx90460-driver.rpi-ws281x
                    ##
                    typing-extensions
                    adafruit-platformdetect
                    adafruit-pureio
                    binho-host-adapter
                    pyserial
                    pyftdi
                    pyusb
                    rpi-gpio
                    sysv-ipc
                    ##
                    flask
                    opencv4
                    psutil
                    icalendar # For generating calendar
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
            python312Env = callPackage ./Nix/pkgs/python312Env.nix {
              extraPackages = with pkgs.python312Packages; [
                flask
                rpi-gpio
                psutil
              ];
            };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { inherit inputs; }; # depends on python312Env
            mlx90460-driver =
              let
                driverPath = "./Nix/pkgs/mlx90460-driver";
              in
              {
                adafruit-circuitpython-mlx90640 =
                  callPackage ./${driverPath}/adafruit-circuitpython-mlx90640.nix
                    { };
                Adafruit-Blinka = callPackage ./${driverPath}/Adafruit-Blinka.nix { };
                adafruit-circuitpython-typing = callPackage ./${driverPath}/adafruit-circuitpython-typing.nix { };
                adafruit-circuitpython-busdevice =
                  callPackage ./${driverPath}/adafruit-circuitpython-busdevice.nix
                    { };
                adafruit-circuitpython-requests =
                  callPackage ./${driverPath}/adafruit-circuitpython-requests.nix
                    { };
                adafruit-circuitpython-connectionmanager =
                  callPackage ./${driverPath}/adafruit-circuitpython-connectionmanager.nix
                    { };
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
