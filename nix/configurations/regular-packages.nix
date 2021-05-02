{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    tdesktop
    google-chrome
    slack
    zulip
    vlc
    texlive.combined.scheme-full
    deja-dup
    bitwarden
    simplescreenrecorder
    spotify
  ];

}
