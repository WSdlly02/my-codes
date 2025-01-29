{
  description = "WSdlly02's Codes Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
          inherit (pkgs) callPackage;
        in
        {
          inC =
            nixpkgs.lib.genAttrs
              [
                # We cannot read file names in Nix
                "agree-cs50"
                "boolean"
                "compare-cs50"
                "discount"
                "float"
                "logical"
                "loops"
                "math"
                "name-cs50"
                "pi"
                "scanf"
                "switch"
                "var"
              ]
              (
                packageName:
                if (nixpkgs.lib.strings.hasSuffix "cs50" packageName) then
                  callPackage ./C {
                    cs50 = true;
                    pname = packageName;
                  }
                else
                  callPackage ./C { pname = packageName; }
              );
          inPython = { };
        }
      );
    };
}
