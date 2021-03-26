{ pkgs, ... }:

let

  master = import (fetchTarball
    https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

in {
  
  programs.home-manager.enable = true;
  home.username = "wanderson";
  home.homeDirectory = "/home/wanderson";
  home.stateVersion = "21.03";
  
  nixpkgs.config.allowUnfree = true;

  imports = [
    ./common/programs.nix
    ./common/home.nix
  ];

  home = {
    packages = with pkgs; [
      (import ../configurations/custom/clojure.nix)
      master.clojure-lsp
      postman
      zoom-us
      awscli
      yarn
      direnv
      nodejs-slim-14_x
    ];
  };
}
