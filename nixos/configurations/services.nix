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
    package = pkgs.postgresql_12;
    enableTCPIP = true;
    port = 54320;
    authentication = pkgs.lib.mkOverride 12 ''
      local all all trust
      host all all ::1/128 trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE wand WITH LOGIN PASSWORD '12345' CREATEDB;
      CREATE DATABASE nixos;
      GRANT ALL PRIVILEGES ON DATABASE nixos TO wand;
    '';
  };
}
