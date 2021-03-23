{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    chromium
    slack
    vlc
    spotify
    libreoffice
    gparted
  ];

}
