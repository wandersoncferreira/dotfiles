{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    slack
    zulip
    vlc
    libreoffice
    simplescreenrecorder
    bitwarden
    texlive.combined.scheme-medium
    p7zip
    dconf
    deja-dup
  ];

}
