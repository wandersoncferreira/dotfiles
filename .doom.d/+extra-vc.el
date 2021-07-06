;;; ../dotfiles/.doom.d/+extra-vc.el -*- lexical-binding: t; -*-

(after! magit-mode
  (setq magit-log-show-gpg-status t
        magit-commit-show-diff nil
        magit-display-buffer-function (lambda (buf) (display-buffer buf '(display-buffer-same-window)))))
