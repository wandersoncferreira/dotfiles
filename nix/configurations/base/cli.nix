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
    htop
    openssl
    oh-my-zsh
    pinentry
    ripgrep
    tree
    unzip
    wget
    xclip
    youtube-dl
    sqlite
    ledger
    file
    patchelf
    ffmpeg
    dmidecode
    dtrx
    graphviz
    pandoc
    silver-searcher
    mpv
    nodePackages.prettier
  ];

  services.openssh.enable = true;

  programs = {
    ssh.startAgent = true;

    gnupg.agent.enable = true;
    gnupg.agent.enableSSHSupport = false;
    gnupg.dirmngr.enable = true;

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
