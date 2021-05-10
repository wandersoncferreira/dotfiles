{ pkgs, lib, ... }:

{
  programs.i3status.enable = true;

  xsession.windowManager.i3 = {
    enable = true;

    config = rec {
      modifier = "Mod4";
      fonts = ["IBM Plex Mono 10"];

      # assigns = {
      #   "1: coding" = [{ class = "^Emacs$"; }];
      # };

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
