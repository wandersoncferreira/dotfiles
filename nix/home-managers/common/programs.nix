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
          askpass="${pkgs.ksshaskpass}/bin/ksshaskpass";
        };
        github = {
          user = "wandersoncferreira";
          oauth-token = "b35352a796fc41f724a6588e4ca19fc2a0cbf068";
        };
        commit = {
          gpgsign = true;
        };
        pull = { rebase = false; };
        rebase = {
          autoStash = false;
          autoSquash = false;
          abbreviateCommands = true;
          missingCommitsCheck = "warn";
        };
        alias = {
          lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%an, %cr)%Creset' --abbrev-commit --date=relative";
          st = "status";
          undo = "reset HEAD~1 --mixed";
          done = "!git push origin HEAD";
          wip = "!git add -u && git commit -m \"WIP\"";
          amend = "commit -a --amend";
          search = "!git rev-list --all | xargs git grep -F";
        };
      };
    };
  };
}
