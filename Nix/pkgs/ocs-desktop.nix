{
  appimageTools,
  fetchurl,
  stdenv,
}:
let
  version = "2.8.5";
  pname = "ocs-desktop";
  src =
    {
      x86_64-linux = fetchurl {
        url = "https://github.com/ocsjs/${pname}/releases/download/${version}/ocs-${version}-setup-linux-x86_64.AppImage";
        hash = "sha256-SujCzoZMZ3YRJCEdrkDmFrYQtvyGAMDW/ragMI/Q+N0=";
      };
      aarch64-linux = fetchurl { };
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
    cp -r ${appimageContents}/usr/lib $out
    cp -r ${appimageContents}/usr/share $out
  '';
}
