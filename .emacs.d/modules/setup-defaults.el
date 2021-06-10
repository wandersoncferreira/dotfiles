;;; setup-defaults --- Changing Emacs defaults
;;; Commentary:
;;; Code:

;; * Attributes

(setq tab-always-indent 'complete)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; stop creating backup files
(setq make-backup-files nil)

;; where emacs customizations via Customize page goes
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

;; truncate lines
(setq-default truncate-lines t)

;; * Functions

(defun bk/eval-buffer ()
  "Provide some feedback after evaluating the buffer."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

;; * Keybindings

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'bk/eval-buffer)

(provide 'setup-defaults)
;;; setup-defaults.el ends here
