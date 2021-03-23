{ pkgs, ... }:

{
  environment.systemPackages = with pkgs;
  [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    curl
    gnumake
    gnupg
    home-manager
    binutils
    stow
    htop
    openssl
    oh-my-zsh
    pinentry
    ripgrep
    tree
    unzip
    vim
    wget
    xclip
    xsel
    youtube-dl
  ];

  services.openssh.enable = true;

  programs = {
    ssh.startAgent = false;
    gnupg.agent.enable = true;
    gnupg.agent.enableSSHSupport = true;

    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" "sudo" "docker"];
      };
      interactiveShellInit = ''
        export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/
        ZSH_THEME="simple"
        plugins=(git sudo docker)

        source $ZSH/oh-my-zsh.sh
      '';
      promptInit = "";
    };
    bash.enableCompletion = true;

  };
}
