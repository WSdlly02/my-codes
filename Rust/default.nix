{
  rustPlatform,
  pname,
}:
rustPlatform.buildRustPackage {
  inherit pname;
  version = "0.0.1";
  src = ./${pname};
  preferLocalBuild = true;
  allowSubstitutes = false;
  cargoLock = {
    lockFile = ./${pname}/Cargo.lock;
  };
}
