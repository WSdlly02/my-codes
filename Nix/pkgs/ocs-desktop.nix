{
  appimageTools,
  fetchurl,
  stdenv,
}:
let
  version = "2.7.1";
  pname = "ocs-desktop";
  src =
    {
      x86_64-linux = fetchurl {
        url = "https://github.com/ocsjs/${pname}/releases/download/${version}/ocs-${version}-setup-linux-x86_64.AppImage";
        hash = "sha256-82zf4Cu+Qh1G1ZF7vUhT4oks9njRaC7KWK+ZNSkrUU4=";
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
