{ lib, config, pkgs, ... }:

let

  dotfilesDir = "$HOME/repos/personal/dotfiles";
  mkTuple = lib.hm.gvariant.mkTuple;

in {

  programs.home-manager.enable = true;

  home.sessionVariables = { BROWSER = "google-chrome-stable"; };

  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "text/html" = "google-chrome.desktop";
      "x-scheme-handler/http" = "google-chrome.desktop";
      "x-scheme-handler/https" = "google-chrome.desktop";
      "x-scheme-handler/mailto" = "hey-mail.desktop";
      "x-scheme-handler/slack" = "slack.desktop";
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      "application/pdf" = "org.gnome.Evince.desktop";
      "image/png" = "org.gnome.eog.desktop";
      "image/jpg" = "org.gnome.eog.desktop";
    };
  };

  home.activation.emacsLink = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${dotfilesDir}/.emacs.d $HOME;
  '';

  dconf.settings = {
    "apps/seahorse/windows/key-manager" = {
      height = 476;
      width = 600;
    };

    "org/gnome/desktop/input-sources" = {
      current = "uint32 1";
      sources = [ (mkTuple [ "xkb" "us" ]) (mkTuple [ "xkb" "br" ]) ];
      xkb-options = [ "ctrl:nocaps" ];
    };

    "org/gnome/desktop/privacy" = {
      disable-microphone = false;
      report-technical-problems = false;
    };

    "org/gnome/desktop/wm/preferences" = {
      button-layout = "icon:minimize,maximize,close";
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "list-view";
      default-sort-order = "type";
      search-filter-time-type = "last_modified";
      search-view = "list-view";
    };

    "org/gnome/settings-daemon/plugins/xsettings" = {
      antialiasing = "grayscale";
      hinting = "slight";
    };

    "org/gnome/system/location" = {
      enabled = true;
    };

    "org/gnome/desktop/background" = {
      picture-uri = "file:///home/wanderson/.local/share/backgrounds/2021-04-03-12-58-21-pexels-pixabay-50594.jpg";
    };

    "org/gnome/desktop/peripherals/keyboard" = {
      repeat-interval = 30;
      repeat = true;
      delay = 400;
    };
    
    "org/gnome/desktop/wm/keybindings" = {
      activate-window-menu=[];
    };

    "org/gnome/shell" = {
      enabled-extensions = [
        "window-list@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
        "places-menu@gnome-shell-extensions.gcampax.github.com"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
      ];
      favorite-apps = [
        "google-chrome.desktop"
        "emacs.desktop"
      ];
    };

    "org/gnome/deja-dup" = {
      exclude-list = ["$TRASH" "$DOWNLOAD"
                      "/home/wanderson/repos" "/home/wanderson/.cache" "/home/wanderson/.clojure"
                      "/home/wanderson/.compose-cache" "/home/wanderson/.config" "/home/wanderson/.elfeed" "/home/wanderson/.gitlibs"
                      "/home/wanderson/.gnupg" "/home/wanderson/.java" "/home/wanderson/.lein" "/home/wanderson/.local"
                      "/home/wanderson/.lsp" "/home/wanderson/.m2" "/home/wanderson/.mozilla" "/home/wanderson/.nix-defexpr"
                      "/home/wanderson/.nix-profile" "/home/wanderson/.npm" "/home/wanderson/.pki" "/home/wanderson/.racket"
                      "/home/wanderson/.ssr" "/home/wanderson/.steam" "/home/wanderson/.vscode" "/home/wanderson/.yarn"
                      "/home/wanderson/.zoom" "/home/wanderson/.zsh"];
      periodic = true;
      periodic-period = 1;
      delete-after = 365;
      backend = "drive";
    };

    "org/gnome/deja-dup/drive" = {
      folder = "wand-x1";
      name = "backup";
      uuid = "F399-1C89";
    };

    "org/gnome/terminal/legacy/profiles:/b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
      use-system-font = false;
      font = "IBM Plex Mono 11";
      audible-bell = false;
      use-theme-colors = true;
    };
  };
  
}  
