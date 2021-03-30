{ lib, config, pkgs, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;

in {

  programs.home-manager.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    extraConfig = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
    '';
    maxCacheTtl = 34560000;
    defaultCacheTtl = 34560000;
  };
}
