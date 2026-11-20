{ pkgs }:
with pkgs;
{
  epson-maintenance = rustPlatform.callPackage ./epson-maintenance.nix { };
  mail-watch = rustPlatform.callPackage ./mail-watch.nix { };

  # Other packages
  ncmdump = callPackage ./ncmdump.nix { };

  # Haskell packages
  haskellEnv = haskellPackages.callPackage ./haskellEnv.nix { };

  # Python packages
  python3Env = python3Packages.callPackage ./python3Env.nix { };
}
