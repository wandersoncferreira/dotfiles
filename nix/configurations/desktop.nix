{ pkgs, config, ... }:

{
  nixpkgs.config.allowBroken = true;

  environment.variables = {
    DOTFILES = "$HOME/repos/personal/dotfiles";
  };

  environment.shellAliases = {
    ls = "ls --color=tty --si";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Sao_Paulo";

  programs.dconf.enable = true;

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

      modules = [ pkgs.xorg.xf86inputlibinput ];

      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
    };
  };

  security.hideProcessInformation = false;
  security.pam.services.sddm.sshAgentAuth = true;

  services = {
    dbus.packages = [ pkgs.gnome3.dconf ];
    udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];
  };

  services.gnome3 = {
    core-shell.enable = true;
    core-utilities.enable = false;
    games.enable = false;
  };

  programs.gnome-terminal.enable = true;
  
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

  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      fira-code
      fira-code-symbols
    ];
  };
}
