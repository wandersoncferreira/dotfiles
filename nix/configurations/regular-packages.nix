{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    slack
    zulip
    vlc
    spotify
    libreoffice
    gparted
    simplescreenrecorder
  ];

}
