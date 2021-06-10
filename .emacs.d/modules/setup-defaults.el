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
(setq widget-image-enable nil)

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
