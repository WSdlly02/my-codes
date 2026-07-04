{
  lib,
  mkShell,
  pkgs,
  extraPkgs ? [ ],
}:
mkShell rec {
  packages =
    with pkgs;
    [
      # openssl
      # sqlite
    ]
    ++ extraPkgs;

  env = {
    LD_LIBRARY_PATH = lib.makeLibraryPath packages;
  };
}
