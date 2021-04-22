{ lib, config, pkgs, ... }:

let

  master = import (fetchTarball
    https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

  mkTuple = lib.hm.gvariant.mkTuple;

in {

  imports = [
    ./common/home.nix
    ./common/programs.nix
  ];

  nixpkgs.config.allowUnfree = true;

  programs = {
    home-manager.enable = true;

    zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      defaultKeymap = "emacs";
      history.extended = true;
      plugins = [
        {
          name = "zsh-autosuggestions";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-autosuggestions";
            rev = "v0.6.3";
            sha256 = "1h8h2mz9wpjpymgl2p7pc146c1jgb3dggpvzwm9ln3in336wl95c";
          };
        }
      ];
    };
  };

  home = {
    username = "wanderson";
    homeDirectory = "/home/wanderson";
    stateVersion = "21.03";
    packages = with pkgs; [
      (import ../configurations/custom/hey.nix)
      master.clojure
      master.rlwrap
      master.leiningen
      master.clj-kondo
      postman
      transmission-qt
      libreoffice

    ];
  };
}
