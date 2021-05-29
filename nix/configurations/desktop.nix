{ pkgs, config, ... }:

{
  nixpkgs.config.allowBroken = true;
  nixpkgs.config.pulseaudio = true;
  environment.shellAliases = {
    rebuild = "sudo nixos-rebuild switch";
    ls = "ls --color=tty --si";
    ll = "ls -l";
    pl = "ps aux | grep ";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Sao_Paulo";
  programs.dconf.enable = true;

  environment.pathsToLink = [ "/libexec" ];

  services = {
    xserver = {
      enable = true;
      layout = "us,br";

      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = true;
      desktopManager.gnome3.enable = true;

      xkbVariant = "intl,abnt2";
      xkbOptions = "ctrl:nocaps";
      videoDrivers = [ "intel" ];

      dbus.packages = [ pkgs.gnome3.dconf ];
      udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

      modules = [ pkgs.xorg.xf86inputlibinput ];

      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # Apps
    gnome3.evince
    gnome3.gnome-tweak-tool
    gnome3.gnome-terminal
    gnome3.nautilus
    gnome3.file-roller
    gnome3.eog
    gnome3.gnome-calculator

    # Extensions
    gnomeExtensions.appindicator
  ];

  services.gnome3 = {
    core-shell.enable = true;
    core-utilities.enable = false;
    games.enable = false;
  };

  programs.gnome-terminal.enable = true;
  security.hideProcessInformation = false;
  services.gnome3.gnome-keyring.enable = true;
  programs.seahorse.enable = true;
  # security.pam.services.lightdm.enableGnomeKeyring = true;

  fonts = {
    fonts = with pkgs; [
      hack-font
      ibm-plex
      roboto-mono
    ];
  };
}
