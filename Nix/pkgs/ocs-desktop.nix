{
  appimageTools,
  fetchurl,
  libarchive,
  stdenv,
}:
let
  version = "2.8.21";
  pname = "ocs-desktop";
  src =
    {
      x86_64-linux = fetchurl {
        url = "https://github.com/ocsjs/${pname}/releases/download/${version}/ocs-${version}-setup-linux-x86_64.AppImage";
        hash = "sha256-2u+8Tmcriz+jLXAWXcqJyc/AV0MW574Kix2BajY0Gt0=";
      };
      aarch64-linux = fetchurl { };
    }
    .${stdenv.hostPlatform.system};
  appimageContents = appimageTools.extract {
    inherit pname version src;
    postExtract = ''
      ${libarchive}/bin/bsdtar -xf $out/resources/bin/chrome/chrome.zip \
        --strip-components=2 \
        -C $out/resources/bin/chrome/
      rm $out/resources/bin/chrome/chrome.zip
    '';
  };
in
appimageTools.wrapAppImage {
  inherit pname version;
  src = appimageContents;
  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/ocs\ desktop.desktop -t $out/share/applications
    substituteInPlace $out/share/applications/ocs\ desktop.desktop \
      --replace 'Exec=AppRun --no-sandbox %U' 'Exec=${pname} --no-sandbox %U'
  '';
}
