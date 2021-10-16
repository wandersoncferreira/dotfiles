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
  (remove-hook 'org-mode-hook #'org-superstar-mode)

  ;; fix shift+<cursor> movements
  ;; Make windmove work in Org mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(setq org-download-method 'directory)

;;; blog
(setq org-hugo-base-dir "~/wandersoncferreira.github.io"
      org-hugo-section "items"
      org-hugo-front-matter-format "yaml")

;; zettelkasten
(if IS-MAC
    (setq org-roam-directory "/Users/wferreir/roam-v2")
  (setq org-roam-directory "/home/wanderson/zettelkasten"))

;; spaced-repetition
(use-package org-fc
  :load-path "~/.doom.d/sources/org-fc"
  :custom (org-fc-directories '("~/roam-v2"))
  :config
  (add-to-list 'org-fc-custom-contexts
               '(security-cards . (:filter (tag "security"))))
  (add-to-list 'org-fc-custom-contexts
               '(comptia-QA . (:filter (tag "comptia-qa")))))
