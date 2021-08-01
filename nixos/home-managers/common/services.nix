{ lib, config, pkgs, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;

in {

  programs.home-manager.enable = true;

  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry}/bin/pinentry-gtk-2
      allow-emacs-pinentry
    '';
    maxCacheTtl = 34560000;
    defaultCacheTtl = 34560000;
  };

  systemd.user.startServices = true;
}
