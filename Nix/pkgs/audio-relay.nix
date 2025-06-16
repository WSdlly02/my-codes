{
  alsa-lib,
  autoPatchelfHook,
  fetchurl,
  file,
  fontconfig,
  freetype,
  libglvnd,
  libpulseaudio,
  makeDesktopItem,
  makeWrapper,
  stdenv,
  xorg,
  zlib,
  zulu17,
}:
stdenv.mkDerivation rec {
  pname = "audio-relay";
  version = "0.27.5";
  src = fetchurl {
    url = "https://dl.audiorelay.net/setups/linux/audiorelay-${version}.tar.gz";
    hash = "sha256-xIVBOaS9Iee/eIGntuIevEz+gjKGeD1Pua1L9O346Mc=";
  };
  sourceRoot = ".";

  desktopItem = makeDesktopItem {
    name = "AudioRelay";
    exec = "audio-relay";
    genericName = "AudioRelay audio bridge";
    comment = "AudioRelay sound server/player";
    categories = [
      "Network"
      "Audio"
    ];
    desktopName = "AudioRelay";
    mimeTypes = [ ];
    icon = "audiorelay";
  };

  installPhase = ''
    mkdir -p $out/share/icons/hicolor/512x512/apps
    ln -sf AudioRelay bin/audio-relay
    cp -rp bin lib $out/
    cp lib/AudioRelay.png $out/share/icons/hicolor/512x512/apps/audiorelay.png
    cp -r ${desktopItem}/share/applications $out/share
    cp $out/lib/app/AudioRelay.cfg $out/lib/app/.AudioRelay-wrapped.cfg
  '';

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [
    alsa-lib
    file
    fontconfig.lib
    freetype
    libglvnd
    libpulseaudio
    stdenv.cc.cc.lib
    xorg.libX11
    xorg.libXext
    xorg.libXi
    xorg.libXrender
    xorg.libXtst
    xorg.libXrandr
    xorg.libXinerama
    zlib
    zulu17
  ];

  dontAutoPatchelf = true;

  postFixup = ''
    autoPatchelf \
      $out/bin \
      $out/lib/runtime/lib/jexec \
      $out/lib/runtime/lib/jspawnhelper \
      $(find "$out/lib/runtime/lib" -type f -name 'lib*.so' -a -not -name 'libj*.so')
    wrapProgram $out/bin/AudioRelay \
      --prefix LD_LIBRARY_PATH : $out/lib/runtime/lib/ \
      --prefix LD_LIBRARY_PATH : ${alsa-lib}/lib/ \
      --prefix LD_LIBRARY_PATH : ${fontconfig.lib}/lib/ \
      --prefix LD_LIBRARY_PATH : ${freetype}/lib/ \
      --prefix LD_LIBRARY_PATH : ${libglvnd}/lib/ \
      --prefix LD_LIBRARY_PATH : ${libpulseaudio}/lib/ \
      --prefix LD_LIBRARY_PATH : ${stdenv.cc.cc.lib}/lib/ \
      --prefix LD_LIBRARY_PATH : ${xorg.libX11}/lib/ \
      --prefix LD_LIBRARY_PATH : ${xorg.libXext}/lib/ \
      --prefix LD_LIBRARY_PATH : ${xorg.libXi}/lib/ \
      --prefix LD_LIBRARY_PATH : ${xorg.libXrender}/lib/ \
      --prefix LD_LIBRARY_PATH : ${xorg.libXtst}/lib/ \
      --prefix LD_LIBRARY_PATH : ${zlib}/lib/ \
      --prefix LD_LIBRARY_PATH : ${zulu17}/lib/ \
      --prefix LD_LIBRARY_PATH : ${zulu17}/lib/server/
  '';
}
