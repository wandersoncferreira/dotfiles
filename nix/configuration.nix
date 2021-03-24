{ config, pkgs, ... }:

{
  imports =
    [
      ./configurations/hardware-configuration.nix
      ./configurations/base/dev.nix
      ./configurations/base/cli.nix
      ./configurations/regular-packages.nix
      ./configurations/desktop.nix
      ./configurations/services.nix
    ];

  nixpkgs.overlays = import ./configurations/overlays.nix;

  nixpkgs.config.allowUnfree = true; 

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;
  networking.networkmanager.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  users.users.wanderson = {
    isNormalUser = true;
    home = "/home/wanderson";
    description = "@bartuka";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "docker"];
  };
    
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
