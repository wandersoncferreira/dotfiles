;;; ../dotfiles/.doom.d/+extra-org.el -*- lexical-binding: t; -*-

(use-package! org
  :init
  (setq org-return-follows-link t
        org-directory "~/org/"
        org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil
        org-agenda-files (list "~/agenda/todo.org"))
  :config
  (remove-hook 'org-mode-hook #'org-superstar-mode))

(setq org-download-method 'directory)

;;; blog
(setq org-hugo-base-dir "~/wandersoncferreira.github.io"
      org-hugo-section "items"
      org-hugo-front-matter-format "yaml")

;; zettelkasten
(if IS-MAC
    (setq org-roam-directory "/Users/wferreir/roam-v2")
  (setq org-roam-directory "/home/wanderson/zettelkasten"))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; spaced-repetition
(use-package org-fc
  :load-path "~/.doom.d/sources/org-fc"
  :custom (org-fc-directories '("~/roam-v2"))
  :config
  (add-to-list 'org-fc-custom-contexts
               '(security-cards . (:filter (tag "security"))))
  (add-to-list 'org-fc-custom-contexts
               '(comptia-QA . (:filter (tag "comptia-qa")))))
