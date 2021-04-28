{ config, stdenv, fetchFromGitHub, pkgs, ... }:

let

  master = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {
    config = config.nixpkgs.config;
  };

in {

  nixpkgs.config.permittedInsecurePackages = [
        "openssl-1.0.2u"
      ];

  environment.systemPackages = with pkgs;
    [
      master.docker-compose
      emacsGcc
      jetbrains.datagrip
      plantuml
      clang
    ];

  virtualisation.docker = {
    enable = true;
    autoPrune = {
      dates = "weekly";
      enable = true;
      flags = [ "--all" ];
    };
  };

  programs = {
    java = {
      enable = true;
      package = pkgs.jdk11;
    };

    adb.enable = true;
  };

  systemd.user.services.emacs = {
    enable = true;
    wantedBy = [ "default.target" ];
    description = "Emacs: the extensible, self-documenting text editor";
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment}; exec ${pkgs.emacsGcc}/bin/emacs --daemon'";
      ExecStop = "${pkgs.emacsGcc}/bin/emacsclient --eval (kill-emacs)";
      Restart = "always";
    };
  };

}
