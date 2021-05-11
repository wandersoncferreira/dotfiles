{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;

    settings = {

      background_opacity = 0.9;

      window = {
        title = "Terminal";

        padding = { y = 5; };
        dimensions = {
          lines = 75;
          columns = 100;
        };
      };

      font = {
        normal.family = "IBM Plex Mono";
        size = 10.0;
      };

      shell = { program = "${pkgs.zsh}/bin/zsh"; };

      # colors = {
      #   primary = {
      #     foreground = "#171421";
      #     background= "#f0fff0";
      #     bright_foreground= "#5e5c64";
      #   };

      #   normal = {
      #     black=   "#171421";
      #     red=     "#c01c28";
      #     green=   "#26a269";
      #     yellow=  "#a2734c";
      #     blue=    "#12488b";
      #     magenta= "#a347ba";
      #     cyan=    "#2aa1b3";
      #     white=   "#d0cfcc";
      #   };

      #   bright = {
      #     black=   "#5e5c64";
      #     red=     "#f66151";
      #     green=   "#33d17a";
      #     yellow=  "#e9ad0c";
      #     blue=    "#2a7bde";
      #     magenta= "#c061cb";
      #     cyan=    "#33c7de";
      #     white=   "#ffffff";
      #   };
      # };
    };
  };
}
