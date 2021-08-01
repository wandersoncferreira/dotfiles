with import <nixpkgs> {};

let
  deps = [
    c-ares
    gtk3-x11
    glib
    libevent
    libdrm
    libvpx
    libxslt
    libnotify
    libappindicator-gtk2
    libappindicator-gtk3
    atk
    mesa
    cups
    systemd
    alsaLib
    at-spi2-atk
    at-spi2-core
    gdk-pixbuf
    pango
    cairo
    xorg.libxcb
    xorg.libX11
    xorg.libXcomposite
    xorg.libXcursor
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXi
    xorg.libXrender
    xorg.libXtst
    xorg.libXrandr
    ffmpeg
    http-parser
    nss
    nspr
    dbus
    expat
  ];

in

stdenv.mkDerivation rec {
  pname = "hey-mail";
  version = "1.1.0";
  rev = "10";

  src = fetchurl {
    url = "https://api.snapcraft.io/api/v1/snaps/download/lfWUNpR7PrPGsDfuxIhVxbj0wZHoH7bK_${rev}.snap";
    sha256 = "4fbd1f4cca1bfe2bd0c57c5df187459f2ce53cf5fd66f8ea155de3b810bfc7ce";
  };

  nativeBuildInputs = [
    squashfsTools
    makeWrapper
  ];

  dontStrip = true;
  dontPatchELF = true;

  unpackPhase = ''
  runHook preUnpack
  unsquashfs "$src"
  cd squashfs-root
  runHook postUnpack
  '';

  installPhase = ''
  runHook preInstall
  
  mkdir -p $out/lib
  mv ./* $out/

  cp $out/meta/snap.yaml $out

  rpath="$out"

  patchelf \
    --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
    --set-rpath $rpath $out/hey-mail

  librarypath="${lib.makeLibraryPath deps}"

  wrapProgram $out/hey-mail \
    --prefix LD_LIBRARY_PATH : "$librarypath" \
    --prefix PATH : $out/bin

  install -Dm755 $out/hey-mail $out/bin/hey-mail

  # fix icon line in the desktop file
  sed -i "s:^Icon=.*:Icon=hey-mail:" "$out/meta/gui/hey-mail.desktop"

  # desktop file
  mkdir -p "$out/share/applications/"
  cp "$out/meta/gui/hey-mail.desktop" "$out/share/applications/"

  # icon
  mkdir -p "$out/share/icons/"
  ln -s "$out/meta/gui/icon.png" "$out/share/icons/hey-mail.png"
  
  runHook postInstall
  '';
}
