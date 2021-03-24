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

      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
      desktopManager.default = "plasma5";
      
      xkbVariant = "intl,abnt2";
      xkbOptions = "ctrl:nocaps";
      videoDrivers = [ "intel" ];

      modules = [ pkgs.xorg.xf86inputlibinput ];

    };
  };

  environment.systemPackages = with pkgs; [
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
