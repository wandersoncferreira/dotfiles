;;; setup-appearance --- Changing Emacs appearance
;;
;;; Commentary:
;;
;;; Code:

;; enable line number modes
(dolist (mode '(prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;;; make cursor the width of the character it is under
(setq x-stretch-cursor t)

;; large fringes to get high-resolution flycheck marks
(fringe-mode '(16 . 0))

;; * Functions

(require 'dash)

(defun bk/search-font (font-name)
  "Search for a FONT-NAME."
  (-filter (lambda (f) (string-match font-name f)) (font-family-list)))

(defun bk/set-ibm-font (size)
  "Set default font at SIZE."
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height size))

(defvar bk--toggle-transparency nil)
(defun bk/toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (if bk--toggle-transparency
      (progn
        (set-frame-parameter (selected-frame) 'alpha 100)
        (setq bk--toggle-transparency nil))
    (progn
      (set-frame-parameter (selected-frame) 'alpha 90)
      (setq bk--toggle-transparency t))))

(defun bk/default-theme ()
  "Default theme to be used."
  (interactive)
  (load-theme 'default-black t)
  (bk/set-ibm-font 110))

(defun bk/dark-theme ()
  "Dark theme option."
  (interactive)
  (load-theme 'zenburn t)
  (bk/set-ibm-font 110))

(defun bk/appearance ()
  "Set of parameters to be used in several places."
  (set-face-attribute 'lazy-highlight nil :background "khaki1")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1")
  (set-background-color "honeydew"))

(defun bk/presentation-theme ()
  "Presentation theme."
  (interactive)
  (bk/set-ibm-font 120)
  (disable-theme 'default-black)
  (bk/appearance))

(defun bk/light-theme ()
  "Light theme default."
  (interactive)
  (disable-theme 'zenburn)
  (set-face-attribute 'mode-line nil :background "grey75" :foreground "black")
  (bk/appearance)
  (bk/set-ibm-font 110))

(setq current-theme '(bk/light-theme))

(defun synchronize-theme ()
  "Choose appropriate theme based on what time is it."
  (setq hour (string-to-number (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 16))
      (setq now '(bk/light-theme))
    (setq now '(bk/dark-theme)))
  (if (equal now current-theme)
      nil
    (setq current-theme now)
    (eval now)))


;; start project with light theme
(bk/light-theme)

;; scheduler to verify if we need to swap between themes
(run-with-timer 0 3600 (lambda () (synchronize-theme)))


;; * External Dependencies

(use-package rainbow-mode
  :ensure t
  :commands
  (rainbow-mode))

(use-package zenburn-theme
  :ensure t)

(provide 'setup-appearance)
;;; setup-appearance.el ends here

