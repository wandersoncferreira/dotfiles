{ pkgs, ... }:

{
  nixpkgs.config.allowBroken = true;

  environment.variables = {
    DOTFILES = "$HOME/repos/personal/dotfiles";
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

    };
  };

  environment.systemPackages = with pkgs; [
    # Apps
    gnome3.gnome-tweak-tool
    gnome3.gnome-sound-recorder
    # Extensions
    gnomeExtensions.paperwm
    gnomeExtensions.appindicator
  ];

  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      fira-code
      fira-code-symbols
      hack-font
    ];
  };
}
