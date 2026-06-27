{
  buildRustPackage,
}:
buildRustPackage {
  pname = "mihomo-updater";
  version = "1.1.1";
  src = ../../SOPs/mihomo-updater;
  cargoLock.lockFile = ../../SOPs/mihomo-updater/Cargo.lock;
  cargoBuildFlags = [
    "--bin"
    "mihomo-updater" # Only build the updater binary
  ];
}
