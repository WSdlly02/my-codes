{
  autoPatchelfHook,
  dpkg,
  fetchurl,
  lib,
  makeWrapper,
  stdenv,
  # Graphics and X11
  libGL,
  libX11,
  libXcomposite,
  libXcursor,
  libXdamage,
  libXext,
  libXfixes,
  libXi,
  libXrandr,
  libXrender,
  libXtst,
  libxcb,
  libxkbfile,
  xorg,
  # System libraries
  alsa-lib,
  at-spi2-atk,
  at-spi2-core,
  atk,
  bash,
  cairo,
  cups,
  dbus,
  expat,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gtk3,
  libdrm,
  libglvnd,
  mesa,
  nspr,
  nss,
  pango,
  systemd,
}:
stdenv.mkDerivation rec {
  pname = "qoder";
  version = "0.2.12-1762247531";

  src = fetchurl {
    url = "https://download.qoder.com/release/latest/qoder_amd64.deb";
    hash = "sha256-W5D5zSy1CtIV/U9LKnulhIs/P4/qKi7Ok2YZ9K3rgnc=";
  };

  sourceRoot = ".";
  dontBuild = true;

  unpackPhase = ''
    runHook preUnpack
    ar -x $src
    tar -xf data.tar.* --warning=no-timestamp
    runHook postUnpack
  '';

  installPhase = ''
    mkdir -p $out/bin $out/share

    # 复制主程序目录
    cp -r usr/share/qoder $out/share/qoder

    # 复制其他资源文件（图标、desktop文件等）
    cp -r usr/share/applications $out/share/applications || true
    cp -r usr/share/appdata $out/share/appdata || true
    cp -r usr/share/pixmaps $out/share/pixmaps || true
    cp -r usr/share/mime $out/share/mime || true
    cp -r usr/share/bash-completion $out/share/bash-completion || true
    cp -r usr/share/zsh $out/share/zsh || true

    # 修正 desktop 文件中的路径
    substituteInPlace $out/share/applications/qoder.desktop \
      --replace-fail '/usr/share/qoder/qoder' "$out/bin/qoder"
    substituteInPlace $out/share/applications/qoder-url-handler.desktop \
      --replace-fail '/usr/share/qoder/qoder' "$out/bin/qoder" || true

    # 创建启动包装脚本
    makeWrapper $out/share/qoder/qoder $out/bin/qoder \
      --prefix LD_LIBRARY_PATH : "$out/share/qoder" \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath buildInputs}
  '';

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    makeWrapper
  ];

  buildInputs = [
    # Audio
    alsa-lib
    # Accessibility
    at-spi2-atk
    at-spi2-core
    atk
    # Graphics and rendering
    cairo
    gdk-pixbuf
    libGL
    libdrm
    libglvnd
    mesa # provides libgbm
    pango
    # X11 libraries
    libX11
    libXcomposite
    libXcursor
    libXdamage
    libXext
    libXfixes
    libXi
    libXrandr
    libXrender
    libXtst
    libxcb
    libxkbfile
    xorg.libxshmfence
    # GTK and dependencies
    gtk3
    glib
    # System libraries
    bash
    cups
    dbus
    expat
    fontconfig
    freetype
    nspr
    nss
    stdenv.cc.cc.lib
    systemd
  ];
  strictDeps = true;
  meta = with lib; {
    description = "Qoder - Agentic Coding Platform for Real Software";
    longDescription = ''
      Qoder is an agentic coding platform designed for real software development.
      Visit https://qoder.com for more information and documentation.
    '';
    homepage = "https://qoder.com";
    license = licenses.unfree;
    platforms = [ "x86_64-linux" ];
    maintainers = [ ];
    mainProgram = "qoder";
  };
}
