;;; ../dotfiles/.doom.d/+extra-appearance.el -*- lexical-binding: t; -*-

(setq doom-theme 'modus-operandi
      doom-font (font-spec :family "Source Code Pro" :size 15)
      display-line-numbers-type nil
      confirm-kill-emacs nil
      fill-column 180
      indent-tabs-mode nil)

;; delete selection
(delete-selection-mode +1)

;; disable from doom
(remove-hook 'doom-first-buffer-hook 'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook 'smartparens-global-mode)

;; fullscreen from beginning
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(defun bk/improve-appearance-default-theme ()
  "Change highlight colors when using the default white theme."
  (set-face-attribute 'lazy-highlight nil :background "khaki1")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))
