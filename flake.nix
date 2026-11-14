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
          overlays = [ inputs.self.overlays.default ] ++ overlays;
        };
      overlays.default =
        final: _prev:
        import ./Nix/pkgs {
          pkgs = final;
        };
    }
    // forExposedSystems (
      system:
      let
        pkgs = pkgs' { inherit system; };
        exposedPackages = import ./Nix/pkgs { inherit pkgs; };
      in
      {
        devShells."${system}" = { };
        formatter."${system}" = pkgs.nixfmt-tree;
        legacyPackages."${system}" = exposedPackages;
      }
    );
}
