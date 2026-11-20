{
  buildRustPackage,
}:
buildRustPackage {
  pname = "mail-watch";
  version = "0.1.0  ";
  src = ../../SOPs/mail-watch;
  cargoLock.lockFile = ../../SOPs/mail-watch/Cargo.lock;
}
