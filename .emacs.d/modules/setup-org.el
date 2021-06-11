;;; setup-org --- Org mode
;;
;;; Commentary:
;;
;;; Code:

(require 'org)
(require 'ob-clojure)
(require 'org-tempo)

;; * Attributes

(setq org-return-follows-link t
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-agenda-files (list "~/agenda/todo.org"))

;; don't ruin S-arrow to switch windows please
(setq org-replace-disputed-keys t) 

;; moving between windows
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; new templates
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))

;; enable languages in src blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ledger . t)
   (clojure . t)))

;; capture TODO tasks
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/agenda/todo.org" "Task")
         "* TODO %^{Title}\n %i" :clock-in t :clock-resume t))
      )

;; disable flycheck in org buffers
(defun disable-flycheck-in-org-src-block ()
  "Disable flycheck inside ORG src blocks."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)

;; * Keybindings

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)


;; * External Dependencies

(use-package edit-indirect
  :ensure t)

(use-package org-download
  :ensure t
  :config
  (setq org-image-actual-width nil))

(provide 'setup-org)
;;; setup-org.el ends here
