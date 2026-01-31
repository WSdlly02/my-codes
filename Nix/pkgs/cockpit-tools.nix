{
  lib,
  stdenv,
  fetchurl,
  dpkg,
  makeWrapper,
  autoPatchelfHook,
  openssl_3,
  gtk3,
  gdk-pixbuf,
  cairo,
  glib,
  webkitgtk_4_1,
  libsoup_3,
  libappindicator-gtk3,
  libayatana-appindicator,
  dbus,
  librsvg,
  # gsettings-desktop-schemas,
}:
let
  version = "0.4.9";
in
stdenv.mkDerivation {
  pname = "cockpit-tools";
  inherit version;

  src = fetchurl {
    url = "https://github.com/jlcodes99/cockpit-tools/releases/download/v${version}/Cockpit.Tools_${version}_amd64.deb";
    sha256 = "3d5d0858046c888c0a016094c2e1e41e1fa63afe0a2eafed13cd9beab36ba9fb";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    makeWrapper
  ];

  buildInputs = [
    # LDD dependencies
    openssl_3
    gtk3
    gdk-pixbuf
    cairo
    glib
    webkitgtk_4_1
    libsoup_3

    # Runtime dependencies (loaded via dlopen)
    libappindicator-gtk3
    libayatana-appindicator

    # Common dependencies
    dbus
    librsvg
  ];

  unpackPhase = ''
    dpkg-deb -x $src .
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share
    cp -r usr/share/* $out/share/
    cp usr/bin/cockpit-tools $out/bin/

    runHook postInstall
  '';

  # Fix for common GTK/GIO issues and runtime library loading (dlopen)
  postFixup = ''
    wrapProgram $out/bin/cockpit-tools \
      --prefix LD_LIBRARY_PATH : "${
        lib.makeLibraryPath [
          libappindicator-gtk3
          libayatana-appindicator
        ]
      }"
  '';

  meta = with lib; {
    description = "Cockpit Tools";
    homepage = "https://github.com/jlcodes99/cockpit-tools";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
    mainProgram = "cockpit-tools";
  };
}
