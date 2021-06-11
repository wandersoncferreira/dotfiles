;;; setup-dired --- Dired mode
;;
;;; Commentary:
;;
;;; Code:

(require 'dired)
(require 'dired-x)

;; * Attributes

(setq dired-listing-switches "-alh")

(setq dired-dwim-target t)

;; enable 'a'-keybinding in dired - which opens the file and closes dired buffer
(put 'dired-find-alternate-file 'disabled nil)


;;  * Keybindings

(use-package dired
  :bind
  (:map dired-mode-map
        ("O" . bk/dired-xdg-open)
        ("M-p" . bk/dired-back-to-top)
        ("M-n" .  bk/dired-back-to-bottom)
        ("C-a" .  bk/dired-back-to-start-of-files)
        ("C-x C-k" . dired-do-delete)
        ("k" . dired-do-delete)))

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

;; * Functions

(defun bk/dired-directories-first ()
  "Sorted dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(advice-add 'dired-readin :after #'bk/dired-directories-first)

(defun bk/dired-xdg-open ()
  "Open the file at point with xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Openning %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun bk/dired-back-to-start-of-files ()
  "Move to start of line."
  (interactive)
  (backward-char (- (current-column) 2)))

(defun bk/dired-back-to-top ()
  "Go back to correct position at the top."
  (interactive)
  (goto-char (point-min))
  (forward-line 2)
  (bk/dired-back-to-start-of-files))

(defun bk/dired-back-to-bottom ()
  "Go back to correct position at the bottom."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (bk/dired-back-to-start-of-files))

(provide 'setup-dired)
;;; setup-dired.el ends here
