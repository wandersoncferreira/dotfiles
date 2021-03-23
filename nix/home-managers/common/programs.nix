{ pkgs, ... }:

{
  # home.stateVersion = "20.09";
  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      package = pkgs.git;
    };
  };
}
