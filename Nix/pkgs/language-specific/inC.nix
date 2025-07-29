{
  callPackage,
  lib,
  stdenv,
}:
lib.genAttrs
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
    "triangle"
    "var"
    "while"
  ]
  (
    packageName:
    callPackage (
      { packageName }:
      let
        pname = packageName;
      in
      stdenv.mkDerivation {
        inherit pname;
        version = "0.0.1";
        src = ../../../C/${pname}.c;
        dontUnpack = true;
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildPhase = ''
          $CC $src -Wall -Werror -std=c17 -o ${pname}
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp ${pname} $out/bin
        '';
        doCheck = false;
      }
    ) { inherit packageName; }
  )
// {
  cOneHundred = lib.genAttrs (map (x: toString x) (lib.range 1 12)) (
    packageName:
    callPackage (
      { packageName }:
      let
        pname = "cOneHundred-" + "${packageName}";
      in
      stdenv.mkDerivation {
        inherit pname;
        version = "0.0.1";
        src = ../../../C/cOneHundred/${packageName}.c;
        dontUnpack = true;
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildPhase = ''
          $CC $src -Wall -Werror -std=c17 -o ${pname}
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp ${pname} $out/bin
        '';
        doCheck = false;
      }
    ) { inherit packageName; }
  );
}
