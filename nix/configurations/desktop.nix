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

  services = {
    xserver = {
      enable = true;
      layout = "us,br";

      desktopManager = {
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };

      displayManager.defaultSession = "xfce+i3";
      displayManager.lightdm.enable = true;

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          dmenu
          i3status
          i3lock
          xfce.terminal
          lxappearance
          arandr
          feh
          evince
        ];
      };

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
  services.gnome3.gnome-keyring.enable = true;
  programs.seahorse.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;

  fonts = {
    fonts = with pkgs; [
      hack-font
      ibm-plex
      roboto-mono
    ];
  };
}
