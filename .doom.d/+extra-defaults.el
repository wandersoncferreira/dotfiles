;;; ../dotfiles/.doom.d/+extra-defaults.el -*- lexical-binding: t; -*-

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wand@hey.com"

      byte-compile-warnings '(cl-functions)
      scroll-margin 2
      enable-local-variables t
      load-prefer-newer t
      show-help-function nil
      read-process-output-max (* 1024 1024)
      dired-listing-switches "-ahl -v"

      projectile-enable-caching nil
      projectile-project-search-path '("~/code"))

;; utf8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; scratch buffer
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode
      initial-major-mode 'emacs-lisp-mode)

;; use single abbrev-table for multiple modes
(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))
