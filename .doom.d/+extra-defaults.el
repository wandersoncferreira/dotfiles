;;; ../dotfiles/.doom.d/+extra-defaults.el -*- lexical-binding: t; -*-

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wand@hey.com"

      byte-compile-warnings '(cl-functions)

      scroll-margin 2
      line-spacing 3

      enable-local-variables t
      messages-buffer-max-lines 10000

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

(add-hook! 'next-error-hook #'recenter)

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace."
  (when (and (eolp) (not (bobp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defadvice load (before debug-log activate)
  (message "Advice: now loading: '%s'" (ad-get-arg 0)))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Single line killed")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no activate region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(ad-activate 'align-regexp)

(defun bk/shame-on-you ()
  (interactive)
  (message "Stop this bad habbit!"))
