{
  buildRustPackage,
}:
buildRustPackage {
  pname = "duplicate-file-finder";
  version = "1.0.0";
  src = ../../SOPs/duplicate-file-finder;
  cargoLock.lockFile = ../../SOPs/duplicate-file-finder/Cargo.lock;
}
