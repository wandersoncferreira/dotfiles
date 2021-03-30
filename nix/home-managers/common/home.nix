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
      "application/pdf" = "org.gnome.Evince.desktop";
      "image/png" = "org.gnome.eog.desktop";
    };
  };

  home.activation.linkFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
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
      search-filter-time-type = "last_modified";
    };

    "org/gnome/settings-daemon/plugins/xsettings" = {
      antialiasing = "grayscale";
      hinting = "slight";
    };

    "org/gnome/system/location" = {
      enabled = true;
    };

    "org/gnome/desktop/peripherals/keyboard" = {
      repeat-interval = 30;
      repeat = true;
      delay = 400;
    };
    
    "org/gnome/desktop/wm/keybindings" = {
      activate-window-menu=[];
    };

    "org/gnome/deja-dup" = {
      exclude-list = ["$TRASH" "$DOWNLOAD" "/home/wanderson/repos"];
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
  };
}
