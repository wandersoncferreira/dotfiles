{ pkgs, config, ... }:

let
  dotfilesDir = "$HOME/repos/personal/dotfiles";
  master = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

in {
  
  programs.home-manager.enable = true;
  home.username = "wanderson";
  home.homeDirectory = "/home/wanderson";
  home.stateVersion = "21.03";
  
  nixpkgs.config.allowUnfree = true;
  
  imports = [
    ./common/programs.nix
  ];

  home = {
    packages = with pkgs; [
      (import ../configurations/custom/clojure.nix)
      clojure-lsp
      postman
      zoom-us
      slack
      awscli
      yarn
    ];

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    
    activation.linkFiles = config.lib.dag.entryAfter ["writeBoundary"] ''

    '';
  };
}
