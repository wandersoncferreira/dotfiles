{ lib, config, pkgs, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;

in {

  imports = [ ];

  nixpkgs.config.allowUnfree = true;

  programs = {
    home-manager.enable = true;
  };

  home = {
    username = "gamer";
    homeDirectory = "/home/gamer";
    stateVersion = "21.03";
    packages = with pkgs; [

    ];
  };

  home.activation = {
    defineBookmarks = config.lib.dag.entryAfter ["writeBoundary"] ''
    '';
  };
}
