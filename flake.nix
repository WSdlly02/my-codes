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
          pkgs = inputs'.nixpkgs.legacyPackages;
          inherit (pkgs)
            callPackage
            mkShell
            writeShellScriptBin
            ;
        in
        {
          devShells = {
            default = mkShell {
              packages = [
                (self'.legacyPackages.python312Env.override {
                  extraPackages = with pkgs.python312Packages; [
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
            python312Env = callPackage ./Nix/pkgs/python312Env.nix { };
            python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { inherit inputs; }; # depends on python312Env
            ####################
            inC =
              nixpkgs.lib.genAttrs
                (
                  [
                    # We cannot read file names in Nix
                    "agree-cs50"
                    "array"
                    "boolean"
                    "compare-cs50"
                    "discount"
                    "float"
                    "for"
                    "logical"
                    "loops"
                    "math"
                    "name-cs50"
                    "pi"
                    "pointer"
                    "project-routine-scheduler"
                    "readcsv"
                    "scanf"
                    "string"
                    "switch"
                    "var"
                    "while"
                  ]
                  ++ nixpkgs.lib.forEach [ 1 2 3 4 6 ] (x: "cOneHundred-${toString x}")
                )
                (
                  packageName:
                  if (nixpkgs.lib.hasSuffix "cs50" packageName) then
                    callPackage ./C {
                      cs50 = true;
                      pname = packageName;
                    }
                  else
                    callPackage ./C { pname = packageName; }
                );
            inPython =
              nixpkgs.lib.genAttrs
                [
                  "class-schedule"
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
                  "hello-world"
                  "guessing-game"
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
