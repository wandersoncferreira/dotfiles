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

}
