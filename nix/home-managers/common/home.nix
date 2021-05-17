{ lib, config, pkgs, ... }:

let

  dotfilesDir = "$HOME/repos/personal/dotfiles";
  mkTuple = lib.hm.gvariant.mkTuple;

in {

  programs.home-manager.enable = true;

  home.sessionVariables = { BROWSER = "google-chrome-stable"; };

  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "text/html" = "google-chrome.desktop";
      "x-scheme-handler/http" = "google-chrome.desktop";
      "x-scheme-handler/https" = "google-chrome.desktop";
      "x-scheme-handler/mailto" = "hey-mail.desktop";
      "x-scheme-handler/slack" = "slack.desktop";
      "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      "application/pdf" = "org.gnome.Evince.desktop";
      "image/png" = "org.gnome.eog.desktop";
      "image/jpg" = "org.gnome.eog.desktop";
    };
  };

  home.activation.defaultHome = config.lib.dag.entryAfter ["writeBoundary"] ''
    #disable annoying notification when changing volume
    kwriteconfig5 --file $HOME/.config/plasmarc --group OSD --key Enabled "false"

    # do not restore desktop session
    kwriteconfig5 --file $HOME/.config/ksmserverrc --group General --key loginMode "default"

    # disable file somthing...
    kwriteconfig5 --file $HOME/.config/baloofilerc --group "Basic Settings" --key Indexing-Enabled "false"

    # enable xrender compositor
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key Backend "XRender"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key Enabled "true"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key GLCore "false"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key GLPreferBufferSwap "a"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key HiddenPreviews "5"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key OpenGLIsUnsafe "false"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key WindowsBlockCompositing "true"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Compositing --key XRenderSmoothScale "false"

    # configure task switch
    kwriteconfig5 --file $HOME/.config/kwinrc --group Effect-CoverSwitch --key TabBox "false"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Effect-CoverSwitch --key TabBoxAlternative "false"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Effect-FlipSwitch --key TabBox "false"
    kwriteconfig5 --file $HOME/.config/kwinrc --group Effect-FlipSwitch --key TabBoxAlternative "false"

    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key ActivitiesMode "1"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key ApplicationsMode "0"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key DesktopMode "1"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key HighlightWindows "false"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key LayoutName "thumbnails"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key MinimizedMode "0"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key ShowDesktopMode "0"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key ShowTabMode "true"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBox --key SwitchingMode "0"

    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key ActivitiesMode "1"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key ApplicationsMode "0"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key DesktopMode "1"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key HighlightWindows "true"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key LayoutName "org.kde.breeze.desktop"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key MinimizedMode "0"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key ShowDesktopMode "0"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key ShowTabMode "true"
    kwriteconfig5 --file $HOME/.config/kwinrc --group TabBoxAlternative --key SwitchingMode "0"

    # touchpad
    kwriteconfig5 --file $HOME/.config/touchpadxlibinputrc --group "Synaptics TM3289-021" --key tapToClick "false"

    # hide files and folders on desktop
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
