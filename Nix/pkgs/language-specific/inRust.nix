{
  lib,
  rustPlatform,
}:
lib.genAttrs
  [
    "fibonacci"
    "guessing-game"
    "hello-world"
    "temperature-converter"
  ]
  (
    packageName:
    rustPlatform.callPackage (
      { packageName }:
      let
        pname = packageName;
      in
      rustPlatform.buildRustPackage {
        inherit pname;
        version = "0.0.1";
        src = ../../../Rust/${pname};
        preferLocalBuild = true;
        allowSubstitutes = false;
        cargoLock = {
          lockFile = ../../../Rust/${pname}/Cargo.lock;
        };
      }
    ) { inherit packageName; }
  )
