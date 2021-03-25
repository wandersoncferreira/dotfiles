{ config, stdenv, fetchFromGitHub, pkgs, ... }:

let
  emacsPackage = ((pkgs.emacsPackagesGen pkgs.emacsGcc).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]));

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
      jetbrains.datagrip
      emacsPackage
      plantuml
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

  services.emacs = with pkgs; {
    enable = true;
    defaultEditor = true;
    package = emacsPackage;
  };
}
