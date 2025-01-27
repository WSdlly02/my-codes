{
  description = "WSdlly02's Codes Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  };

  outputs =
    {
      self,
      nixpkgs,
    }@inputs:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ];
    in
    {
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);

      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          inC = {
            name = pkgs.callPackage ./C/name.nix { };
            compare = pkgs.callPackage ./C/compare.nix { };
            agree = pkgs.callPackage ./C/agree.nix { };
            loops = pkgs.callPackage ./C/loops.nix { };
            var = pkgs.callPackage ./C/var.nix { };
            float = pkgs.callPackage ./C/float.nix { };
            math = pkgs.callPackage ./C/math.nix { };
            pi = pkgs.callPackage ./C/pi.nix { };
          };
          inPython = { };
        }
      );
    };
}
