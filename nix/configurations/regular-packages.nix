{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    vlc
  ];

}
