;;; ../dotfiles/.doom.d/+extra-org.el -*- lexical-binding: t; -*-

(remove-hook 'org-mode-hook #'org-superstar-mode)

(after! org
  (setq org-return-follows-link t
        org-directory "~/org/"
        org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil
        org-agenda-files (list "~/agenda/todo.org")))

(after! org-download
  (setq org-download-method 'directory))

(after! org-roam-server
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 17042
        org-roam-server-export-inline-images t)
  (require 'org-roam-protocol))

;;; blog
(after! ox-hugo
  (setq org-hugo-base-dir "~/wandersoncferreira.github.io"
        org-hugo-section "items"
        org-hugo-front-matter-format "yaml"))


;; zettelkasten
(after! org-roam
  (if IS-MAC
      (setq org-roam-directory "/Users/wferreir/zettelkasten")
    (setq org-roam-directory "/home/wanderson/zettelkasten")))

;; spaced-repetition
(use-package org-fc
  :load-path "~/.doom.d/sources/org-fc"
  :custom (org-fc-directories '("~/zettelkasten")))
