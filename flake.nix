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
        # Currently supported systems
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
          inherit (pkgs)
            callPackage
            writeShellScriptBin
            ;
        in
        {
          python312Env = callPackage ./Nix/pkgs/python312Env.nix { };
          python312FHSEnv = callPackage ./Nix/pkgs/python312FHSEnv.nix { inherit inputs; }; # depends on python312Env
          inC =
            nixpkgs.lib.genAttrs
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
          inPython =
            nixpkgs.lib.genAttrs
              [
                "project-routine-scheduler"
                "roots-resolver"
              ]
              (
                packageName:
                # pkgs.runCommandLocal just running commands without leaving any executable files
                # But nix run .# requires a binary to execute
                # It's just suitable for installing some misc files
                # Shebang will inherit env vars
                # But cannot export $PATH vars
                writeShellScriptBin "${packageName}-wrapper" ''
                  ${inputs.self.packages."${pkgs.system}".python312Env}/bin/python3.12 ./Python/${packageName}.py $@
                ''
              );
        }
      );
    };
}
