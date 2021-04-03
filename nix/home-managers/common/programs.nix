{ pkgs, ... }:

{
  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      package = pkgs.git;
      userName = "Wanderson Ferreira";
      userEmail = "wand@hey.com";
      signing = {
        key = "56840A614DBE37AE";
        signByDefault = true;
      };
      extraConfig = {
        init = { defaultBranch = "main"; };
        core = {
          editor = "emacs";
          quotepath = false;
          askpass="${pkgs.ksshaskpass}/bin/ksshaskpass";
        };
        github = {
          user = "wandersoncferreira";
          oauth-token = "b35352a796fc41f724a6588e4ca19fc2a0cbf068";
        };
        commit = {
          gpgsign = true;
        };
        pull = { rebase = false; };
        rebase = {
          autoStash = false;
          autoSquash = false;
          abbreviateCommands = true;
          missingCommitsCheck = "warn";
        };
      };
    };

    gnome-terminal = {
      enable = true;
      themeVariant = "light";
      profile = {
        "5ddfe964-7ee6-4131-b449-26bdd97518f7" = {
          default = true;
          visibleName = "nixProfile";
          allowBold = true;
          cursorBlinkMode = "off";
          scrollbackLines = 100000;
          showScrollbar = false;
          font = "Fira Code Regular 12";
          colors = {
            backgroundColor = "#FFFFE8";
            foregroundColor = "#424242";
            palette = [
              "#070736364242"
              "#DCDC32322F2F"
              "#858599990000"
              "#B5B589890000"
              "#26268B8BD2D2"
              "#D3D336368282"
              "#2A2AA1A19898"
              "#EEEEE8E8D5D5"
              "#00002B2B3636"
              "#CBCB4B4B1616"
              "#58586E6E7575"
              "#65657B7B8383"
              "#838394949696"
              "#6C6C7171C4C4"
              "#9393A1A1A1A1"
              "#FDFDF6F6E3E3"
            ];
          };
        };
      };
    };
  };
}
