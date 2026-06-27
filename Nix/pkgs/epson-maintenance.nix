{
  buildRustPackage,
}:
buildRustPackage {
  pname = "epson-maintenance";
  version = "1.0.0";
  src = ../../SOPs/epson-maintenance;
  cargoLock.lockFile = ../../SOPs/epson-maintenance/Cargo.lock;
}
