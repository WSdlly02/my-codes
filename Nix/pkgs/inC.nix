{ callPackage, lib }:
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
    callPackage ../../C {
      inherit packageName;
      pname = packageName;
    }
  )
// {
  cOneHundred = lib.genAttrs (map (x: toString x) (lib.range 1 12)) (
    packageName:
    callPackage ../../C {
      inherit packageName;
      pname = "cOneHundred-" + "${packageName}";
    }
  );
}
