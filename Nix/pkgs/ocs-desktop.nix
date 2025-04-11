{
  lib,
  appimageTools,
  fetchurl,
  system,
}:
let
  version = "2.6.24";
  pname = "ocs-desktop";
  src =
    {
      x86_64-linux = fetchurl {
        url = "https://github.com/ocsjs/${pname}/releases/download/2.6.24/ocs-${version}-setup-linux-x86_64.AppImage";
        hash = "sha256-bDeV3V6CgIGQx0iJgkxwnAK0ze2dIeJ22m6rD7BjTtI=";
      };
    }
    .${system};
  appimageContents = appimageTools.extractType1 { inherit pname version src; };
in
appimageTools.wrapType2 rec {
  inherit pname version src;
}
