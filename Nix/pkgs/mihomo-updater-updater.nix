{
  buildGoModule,
}:
buildGoModule {
  pname = "mihomo-updater-updater";
  version = "1.0.0";

  env = {
    CGO_ENABLED = "0";
    GOAMD64 = "v3";
  };
  ldflags = [
    "-s"
    "-w"
  ];
  trimpath = true; # Enabled by default in buildGoModule
  dontFixup = true;

  src = ../../SOPs/mihomo-updater/updater;
  vendorHash = null;
}
