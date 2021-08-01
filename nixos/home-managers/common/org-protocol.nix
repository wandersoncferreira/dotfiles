{ pkgs, ... }:

let
  orgProtocolDesktopItem = pkgs.makeDesktopItem {
    name = "Org-Protocol";
    desktopName = "Org-Protocol";
    exec = "${pkgs.emacsGcc}/bin/emacsclient -n %u";
    terminal = "false";
    type = "Application";
    mimeType = "x-scheme-handler/org-protocol";
  };
in {
  nixpkgs.overlays = import ../../configurations/overlays.nix;
  home.packages = [ orgProtocolDesktopItem ];
}
