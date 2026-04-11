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
      # this function folds over all exposed systems and merges the results
      forExposedSystems = f: lib.foldl' lib.recursiveUpdate { } (map f exposedSystems);
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
          }
          // config;
          overlays = [
            inputs.self.overlays.default
            inputs.self.overlays.exposedPackages
            inputs.self.overlays.libraryPackages
          ]
          ++ overlays;
        };
      overlays = {
        default = final: prev: {
          # Overlays here will be applied to all packages
        };
        exposedPackages =
          # Packages here will be exposed and used as libraries in other parts of the flake
          final: prev:
          (inputs.self.legacyPackages.${prev.stdenv.hostPlatform.system} or { }).exposedPackages or { };
        libraryPackages =
          # Packages here will be used as library but won't be exposed
          final: prev:
          (inputs.self.legacyPackages.${prev.stdenv.hostPlatform.system} or { }).libraryPackages or { };
      };
    }
    // forExposedSystems (
      system: with (pkgs' { inherit system; }); {
        devShells."${system}" = rec {
          default = binEnv;
          binEnv = callPackage ./Nix/devShells-binEnv.nix { };
          binEnvWithRocm = callPackage ./Nix/devShells-binEnv.nix { rocmSupport = true; };
        };
        formatter."${system}" = nixfmt-tree;
        legacyPackages."${system}" = {
          # Packages here won't be exposed and used as a library in other parts of the flake

          exposedPackages = {
            # Packages here will be exposed and used as libraries in other parts of the flake
            mihomo-updater-updater = callPackage ./Nix/pkgs/mihomo-updater-updater.nix { };
            ncmdump = callPackage ./Nix/pkgs/ncmdump.nix { };
            ocs-desktop = callPackage ./Nix/pkgs/ocs-desktop.nix { };
            # Haskell packages
            id-generator = haskellPackages.callPackage ./Nix/pkgs/id-generator.nix { };
            haskellEnv = haskellPackages.callPackage ./Nix/pkgs/haskellEnv.nix { };
            # Python packages
            python3Env = python3Packages.callPackage ./Nix/pkgs/python3Env.nix { };
          };
          libraryPackages = {
            # Packages here will be used as library but won't be exposed
          };
        };
      }
    );
}
