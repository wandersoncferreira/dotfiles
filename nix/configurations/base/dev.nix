{ config, stdenv, fetchFromGitHub, pkgs, ... }:

let
  emacsPackage = ((pkgs.emacsPackagesGen pkgs.emacsGcc).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]));

in {

  # I need this for pgadmin 3
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];


  environment.systemPackages = with pkgs;
    [
      docker-compose
      pgadmin
      emacsPackage
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
