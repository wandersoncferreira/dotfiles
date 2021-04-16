{ pkgs, config, ... }:

{
  nixpkgs.config.allowBroken = true;

  environment.variables = {
    DOTFILES = "$HOME/repos/personal/dotfiles";
  };

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

      displayManager.sddm.enable = true;
      displayManager.defaultSession = "plasma5";
      desktopManager.plasma5.enable = true;

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
  security.pam.services.sddm.enableKwallet = true;
  security.pam.services.kwallet = {
    name = "kwallet";
    enableKwallet = true;
  };

  environment.systemPackages = with pkgs; [
    kwalletmanager
    ksshaskpass
    libsForQt5.kwallet
    ark
    ksysguard
    kwallet-pam
    gwenview
    okular
    kcalc
    kgpg
  ];

  fonts = {
    fonts = with pkgs; [
      hack-font
      ibm-plex
      roboto-mono
    ];
  };
}
