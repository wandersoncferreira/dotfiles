{ pkgs, config, ... }:

let
  dotfilesDir = "$HOME/repos/personal/dotfiles";
  master = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

in {
  
  programs.home-manager.enable = true;
  home.username = "wanderson";
  home.homeDirectory = "/home/wanderson";
  home.stateVersion = "21.03";
  
  nixpkgs.config.allowUnfree = true;

  imports = [
    ./common/programs.nix
    ./common/home.nix
  ];

  home = {
    packages = with pkgs; [
      (import ../configurations/custom/clojure.nix)
      master.clojure-lsp
      postman
      zoom-us
      awscli
      yarn
      direnv
      nodejs-slim-14_x
    ];

    activation.linkFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${dotfilesDir}/face.icon ~/.face.icon

    export GIT_ASKPASS=$SSH_ASKPASS

    #disable annoying notification when changing volume
    kwriteconfig5 --file $HOME/.config/plasmarc --group OSD --key Enabled "false"

    #do not restore desktop session
    kwriteconfig5 --file $HOME/.config/ksmserverrc --group General --key loginMode "default"

    #disable file somthing...
    kwriteconfig5 --file $HOME/.config/baloofilerc --group "Basic Settings" --key Indexing-Enabled "false"

    #touchpad
    kwriteconfig5 --file $HOME/.config/touchpadxlibinputrc --group "SynPS/2 Synaptics TouchPad" --key tapToClick "false"

    #hide files and folders on desktop
    sed -i 's/plugin=org.kde.plasma.folder/plugin=org.kde.desktopcontainment/g' $HOME/.config/plasma-org.kde.plasma.desktop-appletsrc

    # fast keys
    kwriteconfig5 --file $HOME/.config/kcminputrc --group Keyboard --key KeyboardRepeating "0"
    kwriteconfig5 --file $HOME/.config/kcminputrc --group Keyboard --key RepeatDelay "300"
    kwriteconfig5 --file $HOME/.config/kcminputrc --group Keyboard --key RepeatRate "40"

    # shortcuts
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group krunner.desktop --key "_launch" "none"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "FullScreenScreenShot" "none"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "_launch" "none,none,Launch Spectacle"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "RectangularRegionScreenShot" "Shift+Print"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "CurrentMonitorScreenShot" "Print"

    # disable shortcuts in conflict with Emacs
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group kwin --key "Activate Window Demanding Attention" "none"

    '';
  };
}
