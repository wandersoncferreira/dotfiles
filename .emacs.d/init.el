;;; init.el --- Wand's config  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <wand@hey.com>
;; URL: https://github.com/wandersoncferreira/dotfiles
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Here be dragons!
;;
;;; Code:

;;; Security

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (ignore param)
     (list start end)))

;; elisp configuration folder
(setq modules-dir (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path modules-dir)

;; configurations specific to places I work at
(setq work-dir (expand-file-name "work" user-emacs-directory))
(add-to-list 'load-path work-dir)

(set-register ?t '(file . "/home/wanderson/agenda/todo.org"))
(set-register ?m '(file . "~/.emacs.d/manual.org"))
(set-register ?l '(file . "/home/wanderson/ledger"))
(set-register ?e '(file . "/home/wanderson/.emacs.d/init.el"))

;; * Packages

(use-package smex :ensure t)

;; * Imports

(require 'setup-defaults)
(require 'setup-keybindings)
(require 'setup-appearance)
(require 'setup-presentation)
(require 'setup-completion)
(require 'setup-dired)
(require 'setup-git)
(require 'setup-org)
(require 'setup-programming)
(require 'setup-search)
(require 'setup-eshell)
(require 'setup-auth)
(require 'setup-projects)
(require 'setup-editor)
(require 'setup-spell)
(require 'setup-window)
(require 'setup-media)
(require 'setup-rest)

;; workplace
(require 'setup-work)
(require 'reifyhealth)
(require 'captalys)
(require 'appsauce)

;; third-party apps
(require 'setup-finance)
(require 'setup-chat)
(require 'setup-zettelkasten)
(require 'setup-rss)
(require 'setup-launcher)

;; modes
(require 'setup-markdown)
(require 'setup-epub)
(require 'setup-pdf)
(require 'setup-nix)

;; languages
(require 'setup-clojure)
(require 'setup-java)
(require 'setup-python)
(require 'setup-typescript)
(require 'setup-sql)
(require 'setup-racket)
(require 'setup-lisp)
(require 'setup-xml)
(require 'setup-plantuml)

(use-package crux
  :ensure t
  :config
  (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c d l") #'crux-duplicate-current-line-or-region)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package eldoc
  :diminish eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil)
  :config
  (global-eldoc-mode +1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; Custom Functions

(defun bk/days-since ()
  "Number of days since DATE to today."
  (interactive)
  (let* ((date (read-from-minibuffer "Inicial date: "))
         (date-zoned (format "%s +0300" date)))
    (message (format "%s days ago" (days-between
                                    (current-time-string)
                                    date-zoned)))))

(defun bk/kill-all-comments ()
  "Function to kill all comments in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun bk/insert-date-today ()
  "Insert today date as YYYY-MM-DD."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (kill-new today)
    (insert today)))

(defun bk/ip ()
  "Find my current public IP address."
  (interactive)
  (let* ((endpoint "https://api.ipify.org")
         (myip (with-current-buffer (url-retrieve-synchronously endpoint)
                 (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))
    (kill-new myip)
    (message "IP: %s" myip)))

;;; Emacs Server

(require 'server)
(unless (server-running-p)
  (server-start))

(defun bk/server-shutdown ()
  "Save buffers, quit, and shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))


;;; End of file

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved cl-functions)
;; End:

;;; init.el ends here
