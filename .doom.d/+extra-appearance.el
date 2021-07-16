;;; ../dotfiles/.doom.d/+extra-appearance.el -*- lexical-binding: t; -*-

(setq doom-theme 'nil
      display-line-numbers-type nil
      confirm-kill-emacs nil
      fill-column 180
      indent-tabs-mode nil)

(when IS-LINUX
  (setq doom-font (font-spec :family "Source Code Pro" :size 15))
  (setq doom-theme 'modus-operandi))

;; delete selection
(delete-selection-mode +1)

;; disable from doom
(remove-hook 'doom-first-buffer-hook 'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook 'smartparens-global-mode)

(defun bk/improve-appearance-default-theme ()
  "Change highlight colors when using the default white theme."
  (set-face-attribute 'lazy-highlight nil :background "khaki1")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1")
  (when IS-MAC
    (set-face-attribute 'default nil :height 140)))

(bk/improve-appearance-default-theme)
