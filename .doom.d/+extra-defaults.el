;;; ../dotfiles/.doom.d/+extra-defaults.el -*- lexical-binding: t; -*-

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wand@hey.com"
      tab-always-indent 'complete
      confirm-kill-emacs nil
      byte-compile-warnings '(cl-functions)
      enable-local-variables t

      ;; spell
      ispell-dictionary "en"

      ;; editor
      fill-column 180
      indent-tabs-mode nil
      which-key-idle-delay 0.5

      ;; auth
      auth-source-debug t
      auth-sources '((:source "~/.secrets/authinfo.gpg"))

      ;; appearance
      doom-theme 'nil
      doom-font (font-spec :family "Source Code Pro" :size 13)
      display-line-numbers-type nil)

;; disable from doom
(remove-hook 'doom-first-buffer-hook 'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook 'smartparens-global-mode)
(remove-hook 'org-mode-hook #'org-superstar-mode)

;; shift arrow to move between buffers
(windmove-default-keybindings)
