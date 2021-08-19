;;; ../dotfiles/.doom.d/+extra-appearance.el -*- lexical-binding: t; -*-

(setq doom-theme 'alect-light-alt
      display-line-numbers-type nil
      confirm-kill-emacs nil
      fill-column 180
      indent-tabs-mode nil)

(when IS-LINUX
  (setq doom-font (font-spec :family "Source Code Pro" :size 15))
  (setq doom-theme 'modus-operandi))

(when IS-MAC
  (set-face-attribute 'default nil :height 140))

;; delete selection
(delete-selection-mode +1)

;; disable from doom
(remove-hook 'doom-first-buffer-hook 'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook 'smartparens-global-mode)

(defun bk/default-theme ()
  "Change highlight colors when using the default white theme."
  (set-face-attribute 'lazy-highlight nil :background "khaki1")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

(defun bk/slate-gray-theme ()
  "Slate Grat"
  (interactive)
  (set-background-color "DarkSlateGray")
  (set-face-background 'mode-line "Wheat")
  (set-face-foreground 'mode-line "DarkSlateGray")
  (set-foreground-color "Wheat"))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; hacky: avoid saving custom faces in custom.el files
(defun custom-save-faces ()
  "No-op"
  (interactive))

(defun bk/alect-themes-customizations ()
  (custom-set-faces
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(font-lock-variable-name-face ((t (:foreground "#6a621b"))))
   '(ivy-current-match ((t (:extend t :background "#b0d0f3" :foreground "#101010" :weight bold))))))

(defun bk/organic-green-customizations ()
  (setq organic-green-boldless t))

(bk/alect-themes-customizations)
