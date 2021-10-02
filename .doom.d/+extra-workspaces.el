;;; ../dotfiles/.doom.d/+extra-workspaces.el -*- lexical-binding: t; -*-

(after! persp-mode
  (setq +workspaces-on-switch-project-behavior t
        persp-autokill-persp-when-removed-last-buffer t))
