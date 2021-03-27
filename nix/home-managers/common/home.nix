{ config, pkgs, ... }:

let

  dotfilesDir = "$HOME/repos/personal/dotfiles";

in {
  
  home.sessionVariables = { BROWSER = "google-chrome"; };

  home.activation.linkFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${dotfilesDir}/face.icon ~/.face.icon

    export GIT_ASKPASS=$SSH_ASKPASS

    #disable annoying notification when changing volume
    kwriteconfig5 --file $HOME/.config/plasmarc --group OSD --key Enabled "false"

    #do not restore desktop session
    kwriteconfig5 --file $HOME/.config/ksmserverrc --group General --key loginMode "default"

    #disable file somthing...
    kwriteconfig5 --file $HOME/.config/baloofilerc --group "Basic Settings" --key Indexing-Enabled "false"

    #touchpad
    kwriteconfig5 --file $HOME/.config/touchpadxlibinputrc --group "Synaptics TM3289-021" --key tapToClick "false"

    #hide files and folders on desktop
    sed -i 's/plugin=org.kde.plasma.folder/plugin=org.kde.desktopcontainment/g' $HOME/.config/plasma-org.kde.plasma.desktop-appletsrc

    # fast keys
    kwriteconfig5 --file $HOME/.config/kcminputrc --group Keyboard --key KeyboardRepeating "0"
    kwriteconfig5 --file $HOME/.config/kcminputrc --group Keyboard --key RepeatDelay "300"
    kwriteconfig5 --file $HOME/.config/kcminputrc --group Keyboard --key RepeatRate "40"

    # shortcuts
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group krunner.desktop --key "_launch" "none"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "FullScreenScreenShot" "none,none,Capture Entire Desktop"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "_launch" "none,none,Launch Spectacle"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "RectangularRegionScreenShot" "Shift+Print,Shift+Print,Capture Rectangular Region"
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group org.kde.spectacle.desktop --key "CurrentMonitorScreenShot" "Print,Print,Capture Current Monitor"

    # disable shortcuts in conflict with Emacs
    kwriteconfig5 --file $HOME/.config/kglobalshortcutsrc --group kwin --key "Activate Window Demanding Attention" "none,none,Activate Window Demanding Attention"

    '';
}
