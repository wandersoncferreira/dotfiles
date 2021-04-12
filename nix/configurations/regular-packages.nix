{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    slack
    zulip
    vlc
    bitwarden
    texlive.combined.scheme-medium
    deja-dup
    simplescreenrecorder
    transmission-qt
    p7zip
    libreoffice
  ];

}
