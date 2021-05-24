{ lib, config, pkgs, ... }:

let

  dotfilesDir = "$HOME/repos/personal/dotfiles";

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


  home.activation.defaultHome = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${dotfilesDir}/i3/config $HOME/.config/i3/config

    '';
}
