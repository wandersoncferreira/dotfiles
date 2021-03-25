{ pkgs, ... }:

{
  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      package = pkgs.git;
      userName = "Wanderson Ferreira";
      userEmail = "wand@hey.com";
      signing = {
        key = "56840A614DBE37AE";
        signByDefault = true;
      };
      extraConfig = {
        init = { defaultBranch = "main"; };
        core = {
          editor = "emacs";
          quotepath = false;
        };
        github = {
          user = "wandersoncferreira";
          oauth-token = "b35352a796fc41f724a6588e4ca19fc2a0cbf068";
        };
        commit = {
          gpgsign = true;
        };
        pull = { rebase = true; };
        rebase = {
          autoStash = true;
          autoSquash = false;
          abbreviateCommands = true;
          missingCommitsCheck = "warn";
        };
      };
    };

    command-not-found.enable = true;

  };
}
