{ pkgs, config, ... }:

let
  dotfilesDir = "$HOME/repos/personal/dotfiles";
in {
  environment.systemPackages = with pkgs;
  [
    postgresql
  ];

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql;
    enableTCPIP = true;
    port = 54320;
  };
}
