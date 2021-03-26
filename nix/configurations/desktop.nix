{ pkgs, config, ... }:

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
      displayManager.defaultSession = "plasma5";
      
      xkbVariant = "intl,abnt2";
      xkbOptions = "ctrl:nocaps";
      videoDrivers = [ "intel" ];

      modules = [ pkgs.xorg.xf86inputlibinput ];

    };
  };

  security.pam.services.sddm.enableKwallet = true;
  security.pam.services.sddm.sshAgentAuth = true;

  environment.systemPackages = with pkgs; [
    kwalletmanager
    ksshaskpass
    okular
  ];

  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      fira-code
      fira-code-symbols
    ];
  };
}
