{
  description = "WSdlly02's Codes Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  } @ inputs: {
    packages = {
      "x86_64-linux" = let
        pkgs = import nixpkgs {system = "x86_64-linux";};
      in {
        inC = {
          name = pkgs.callPackage ./C/name.nix {};
          compare = pkgs.callPackage ./C/compare.nix {};
          agree = pkgs.callPackage ./C/agree.nix {};
          loops = pkgs.callPackage ./C/loops.nix {};
        };
        inPython = {};
      };
    };
  };
}
