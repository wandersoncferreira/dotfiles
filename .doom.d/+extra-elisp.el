;;; ../dotfiles/.doom.d/+extra-elisp.el -*- lexical-binding: t; -*-

(after! elisp-mode
  (add-hook! 'emacs-lisp-mode-hook (enable-paredit-mode))
  (remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
