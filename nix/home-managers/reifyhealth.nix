{ lib, config, pkgs, ... }:

let

  master = import (fetchTarball
    https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

  dotfilesDir = "$HOME/repos/personal/dotfiles";

in {

  imports = [
    ./common/programs.nix
    ./common/home.nix
    ./common/services.nix
    ./common/alacritty.nix
    ./common/i3.nix
    ./common/compton.nix
    ./common/org-protocol.nix
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
      shellAliases = {
        esource = "cd $HOME/repos/reifyhealth/esource-service/";
        study-sheet = "cd $HOME/repos/reifyhealth/study-sheet/";
        reifykey = "ssh-add $HOME/.secrets/keys/id_rsa";
      };
      envExtra = ''
      export PATH="/home/wanderson/repos/personal/dotfiles/scripts:$PATH"

      '';
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };
  };


  home = {
    username = "wanderson";
    homeDirectory = "/home/wanderson";
    stateVersion = "21.03";
    packages = with pkgs; [
      (import ../configurations/custom/clojure.nix)
      (import ../configurations/custom/hey.nix)
      master.clojure-lsp
      master.rlwrap
      master.babashka
      zoom-us
      yarn
      direnv
      nodejs-14_x
      icu67                     # patch embedded postgresql used in eSource
    ];
  };

  home.activation.linkFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${dotfilesDir}/face.icon ~/.face.icon
    
    mkdir -p $HOME/.clojure
    mkdir -p $HOME/.config/clj-kondo
    mkdir -p $HOME/.lsp

    ln -sf ${dotfilesDir}/clojure/deps.edn $HOME/.clojure/deps.edn
    ln -sf ${dotfilesDir}/clojure/rebl-0.9.242.jar $HOME/.clojure/rebl-0.9.242.jar
    ln -sf ${dotfilesDir}/clojure/clj-kondo/config.edn $HOME/.config/clj-kondo/config.edn
    ln -sf ${dotfilesDir}/clojure/lsp/config.edn $HOME/.lsp/config.edn

    export GIT_ASKPASS=$SSH_ASKPASS

    ssh-add $HOME/.secrets/keys/id_rsa

    '';

}
