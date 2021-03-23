{ pkgs, ... }:

{
  # home.stateVersion = "20.09";
  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      package = pkgs.git;
      userName = "Wanderson Ferreira";
      userEmail = "wand@hey.com";
      signing = {
        key = "wand@hey.com";
        signByDefault = true;
      };
    };

    programs.command-not-found.enable = true;

  };
}
