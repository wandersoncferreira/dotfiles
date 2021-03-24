{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    chromium
    slack
    zulip
    vlc
    spotify
    libreoffice
    gparted
    simplescreenrecorder
  ];

}
