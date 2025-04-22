{
  lib,
  appimageTools,
  fetchurl,
  stdenv,
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
    .${stdenv.hostPlatform.system};
  appimageContents = appimageTools.extract { inherit pname version src; };
in
appimageTools.wrapType2 rec {
  inherit pname version src;

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/ocs\ desktop.desktop -t $out/share/applications
    substituteInPlace $out/share/applications/ocs\ desktop.desktop \
      --replace 'Exec=AppRun --no-sandbox %U' 'Exec=${pname} --no-sandbox %U'
    cp -r ${appimageContents}/usr/share/icons $out/share
  '';
}
