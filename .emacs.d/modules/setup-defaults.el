;;; setup-defaults --- Changing Emacs defaults
;;; Commentary:
;;; Code:

;; * Aliases

(defalias 'yes-or-no-p 'y-or-n-p)

;; * Attributes

(setq tab-always-indent 'complete)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/Trash")

;; always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; lines should be 80 characters wide, not 72
(setq fill-column 80)
(setq fci-rule-column 80)

;; show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; sentences do not need double spaces to end
(set-default 'sentence-end-double-space nil)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; stop creating backup files
(setq make-backup-files nil)

;; where emacs customizations via Customize page goes
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; stop using tabs
(setq-default indent-tabs-mode nil)

;; handling long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; disable dialog boxes
(setq use-dialog-box nil)

;; disable file dialog
(setq use-file-dialog nil)

;; no ugly button for checkboxes
(require 'wid-edit)
(setq widget-image-enable nil)

;; more complex clipboard management
(setq save-interprogram-paste-before-kill t)

;;; real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; don't warn for following symlinked files
(setq vc-follow-symlinks t)

;; ellispis are nice
(require 'mule-util)
(setq truncate-string-ellipsis "…")

;; don't warn when advice is added for functions
(setq ad-redefinition-action 'accept)

;; user configs
(setq user-mail-address "wand@hey.com"
      user-full-name "Wanderson Ferreira")

(require 'time)
(setq world-clock-list
      '(("Etc/UTC" "UTC")
        ("America/Sao_Paulo" "Sao Paulo")
        ("Europe/Paris" "Paris")
        ("America/Boston" "Boston")))

(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-lisp-eval-depth (* 15 max-lisp-eval-depth))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(delete-selection-mode +1)

;; truncate lines
(setq-default truncate-lines t)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w"))

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

;; * Functions

(defun bk/eval-buffer ()
  "Provide some feedback after evaluating the buffer."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

;; * Keybindings

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'bk/eval-buffer)

;; * External Dependencies

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package vlf :ensure t)

(provide 'setup-defaults)
;;; setup-defaults.el ends here
