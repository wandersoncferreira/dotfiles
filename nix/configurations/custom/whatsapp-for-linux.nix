with import <nixpkgs> {};

stdenv.mkDerivation rec {

  pname = "whatsapp-for-linux";
  version = "1.1.5";

  src = fetchFromGitHub {
    owner = "eneshecan";
    repo = pname;
    rev = "v${version}";
    sha256 = "1gzahls4givd2kbjdwx6yb3jv7a3r1krw40qihiz7hkamkrpaiaz";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    glib-networking
  ];

  buildInputs = [
    gnome3.gtkmm
    gnome3.webkitgtk
    libappindicator-gtk3
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-libav
    pcre
    glib-networking
  ];
}
