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
(require 'setup-custom-functions)

;; Workplace
(require 'setup-work)
(require 'reifyhealth)
(require 'captalys)
(require 'appsauce)

;; Third-Party Apps
(require 'setup-finance)
(require 'setup-chat)
(require 'setup-zettelkasten)
(require 'setup-rss)
(require 'setup-launcher)

;; Modes
(require 'setup-markdown)
(require 'setup-epub)
(require 'setup-pdf)
(require 'setup-nix)

;; Languages
(require 'setup-clojure)
(require 'setup-java)
(require 'setup-python)
(require 'setup-typescript)
(require 'setup-sql)
(require 'setup-racket)
(require 'setup-lisp)
(require 'setup-xml)
(require 'setup-plantuml)

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
