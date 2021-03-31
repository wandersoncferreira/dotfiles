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
    dconf
    deja-dup
  ];

}
