{ config, stdenv, fetchFromGitHub, pkgs, ... }:

let
  emacsPackage = ((pkgs.emacsPackagesGen pkgs.emacsGcc).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]));

  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;

in {

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  # I need this for pgadmin 3
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];


  environment.systemPackages = with pkgs;
    [
      (import ./custom/clojure.nix)
      docker-compose
      pgadmin
      emacsPackage
    ];

  programs = {
    java = {
      enable = true;
      package = pkgs.jdk11;
    };

    adb.enable = true;
  };
}
