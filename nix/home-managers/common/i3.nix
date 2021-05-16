{ pkgs, lib, ... }:

{
  services.screen-locker.lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";

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

  xsession.windowManager.i3 = {
    enable = true;

    config = rec {
      modifier = "Mod4";
      fonts = ["IBM Plex Mono 10"];

      # assigns = {
      #   "1: coding" = [{ class = "^Emacs$"; }];
      # };

      bars = [
        {
          fonts = [ "IBM Plex Mono 10" ];
        }
      ];

      floating.criteria = [
        { class = "Pavucontrol"; }
        { class = "zoom"; instance = "zoom"; }
      ];


      keybindings = lib.mkOptionDefault {
        "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
        "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -modi drun -show drun";
        "${modifier}+Shift+d" = "exec ${pkgs.rofi}/bin/rofi -show window";
        "${modifier}+b" = "exec ${pkgs.google-chrome}/bin/google-chrome-stable";
        "${modifier}+Shift+x" = "exec systemctl suspend";
      };

      startup = [
        {
          command = "exec i3-msg workspace 1";
          always = true;
          notification = false;
        }

        {
          command = "feh --bg-fill ~/Pictures/pexels-pixabay-50594.jpg --bg-fill ~/Pictures/pexels-roberto-nickson-2559941.jpg";
          always = true;
          notification = false;
        }

        {
          command = "xset r rate 300 40";
          always = true;
          notification = false;
        }

        {
          command = "systemctl --user start emacs.service";
          always = true;
          notification = false;
        }

        {
          command = "~/.screenlayout/workstation.sh";
          always = true;
          notification = false;
        }
      ];
    };
  };
}
