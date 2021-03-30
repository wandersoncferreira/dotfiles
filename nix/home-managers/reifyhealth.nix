{ lib, config, pkgs, ... }:

let

  master = import (fetchTarball
    https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

  mkTuple = lib.hm.gvariant.mkTuple;

  dotfilesDir = "$HOME/repos/personal/dotfiles";

in {

  imports = [
    ./common/programs.nix
    ./common/home.nix
    ./common/services.nix
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
      master.whatsapp-for-linux
      master.clojure-lsp
      zoom-us
      yarn
      direnv
      nodejs-slim-14_x
    ];
  };

  dconf.settings = {

    "org/gnome/desktop/background" = {
      picture-options = "zoom";
      picture-uri = "file:///nix/store/8yzca1b0bwdxy65gfxhxlp88i40hxmws-gnome-backgrounds-3.36.0/share/backgrounds/gnome/Acrylic.jpg";
      primary-color = "#ffffff";
      secondary-color = "#000000";
    };

    "org/gnome/desktop/interface" = {
      cursor-theme = "breeze_cursors";
      enable-animations = false;
      font-name = "Noto Sans,  10";
      gtk-im-module = "gtk-im-context-simple";
      gtk-key-theme = "Emacs";
      gtk-theme = "Adwaita";
      icon-theme = "breeze";
      toolbar-style = "both-horiz";
    };

    "org/gnome/desktop/notifications" = {
      application-children = [ "google-chrome" "gnome-power-panel" "gnome-network-panel" ];
      show-banners = false;
    };

    "org/gnome/desktop/notifications/application/gnome-network-panel" = {
      application-id = "gnome-network-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/gnome-power-panel" = {
      application-id = "gnome-power-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/google-chrome" = {
      application-id = "google-chrome.desktop";
    };

    "org/gnome/desktop/screensaver" = {
      picture-options = "zoom";
      picture-uri = "file:///nix/store/8yzca1b0bwdxy65gfxhxlp88i40hxmws-gnome-backgrounds-3.36.0/share/backgrounds/gnome/Acrylic.jpg";
      primary-color = "#ffffff";
      secondary-color = "#000000";
    };

    "org/gnome/desktop/search-providers" = {
      disable-external = true;
      sort-order = [ "org.gnome.Documents.desktop" "org.gnome.Nautilus.desktop" ];
    };

    "org/gnome/nautilus/window-state" = {
      initial-size = mkTuple [ 1280 1333 ];
      maximized = false;
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
    };

    "org/gnome/shell" = {
      enabled-extensions = [
        "window-list@gnome-shell-extensions.gcampax.github.com"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
      ];

      command-history = [ "gnome-tweaks" ];
      favorite-apps = [
        "emacs.desktop"
        "google-chrome.desktop"
        "terminal.desktop"
        "slack.desktop"
        "Zoom.desktop"
      ];
    };

    "org/gnome/shell/extensions/window-list" = {
      display-all-workspaces = false;
      show-on-all-monitors = true;
      grouping-mode = "auto";
    };
  };

  home.activation = {
    defineBookmarks = config.lib.dag.entryAfter ["writeBoundary"] ''
      ln -sf ${dotfilesDir}/config/gtk-3.0/bookmarks $HOME/.config/gtk-3.0/bookmarks

      mkdir -p $HOME/.clojure
      mkdir -p $HOME/.config/clj-kondo
      mkdir -p $HOME/.lsp

      ln -sf ${dotfilesDir}/clojure/deps.edn $HOME/.clojure/deps.edn
      ln -sf ${dotfilesDir}/clojure/rebl-0.9.242.jar $HOME/.clojure/rebl-0.9.242.jar
      ln -sf ${dotfilesDir}/clojure/clj-kondo/config.edn $HOME/.config/clj-kondo/config.edn
      ln -sf ${dotfilesDir}/clojure/lsp/config.edn $HOME/.lsp/config.edn
    '';
  };
}
