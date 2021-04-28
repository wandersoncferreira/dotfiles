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
      "application/pdf" = "org.gnome.Evince.desktop";
      "image/png" = "org.gnome.eog.desktop";
      "image/jpg" = "org.gnome.eog.desktop";
    };
  };

  programs.i3status = {
    enable = true;
    enableDefault = false;
    modules = {
      "volume master" = {
        position = 1;
        settings = {
          format = "♪ %volume";
          format_muted = "♪ muted (%volume)";
          device = "pulse";
        };
      };

      "disk /" = {
        position = 2;
        settings = {
          format = "SSD: %avail";
          low_threshold = 10;
          threshold_type = "percentage_avail";
        };
      };

      "battery 0" = {
        position = 3;
        settings = {
          format = "%status %percentage (%remaining)";
          format_down = "No battery";
          status_chr = "⚇";
          status_bat = "⚡";
          status_full = "☻";
          path = "/sys/class/power_supply/BAT%d/uevent";
          threshold_type = "percentage";
          low_threshold = 20;
          hide_seconds = true;
          last_full_capacity = true;
          integer_battery_capacity = true;
        };
      };

      "cpu_usage" = {
        position = 4;
        settings = {
          format = "CPU: %usage";
        };
      };

      "ethernet enp0s20f0u2u3" = {
        position = 5;
        settings = {
          format_up = "E: %ip (%speed)";
          format_down = "";
        };
      };

      "wireless wlp2s0" = {
        position = 6;
        settings = {
          format_up = "W: %essid, %bitrate";
          format_down = "";
        };
      };

      "tztime local" = {
        position = 7;
        settings = {
          format = "%d/%m %H:%M";
        };
      };
    };

  };

  services.screen-locker.lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";

  home.activation.defaultHome = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${dotfilesDir}/i3/config $HOME/.config/i3/config

    '';
}
