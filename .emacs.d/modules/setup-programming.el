;;; setup-programming --- Emacs general programming mode
;;
;;; Commentary:
;;
;;; Code:

;; * Functions


(defun yas/goto-end-of-active-field ()
  "End of the field."
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  "Start of the field."
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))


;; * External Dependencies

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode +1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode +1))

(use-package toggle-test
  :ensure t
  :init
  (setq tgt-open-in-new-window nil)
  :config
  (put 'tgt-projects 'safe-local-variable #'lisp)
  (global-set-key (kbd "H-s-t") 'tgt-toggle))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-checker-error-threshold 4000))

(use-package flycheck-clj-kondo :ensure t)

(use-package flycheck-projectile :ensure t)

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode +1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-verbosity 1
        yas-wrap-around-region t)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)

  ;; jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  :bind
  (("C-x y" . yas-expand)
   ("C-c t" . yas-describe-tables)))

(use-package yasnippet-snippets :ensure t)

(use-package quickrun :ensure t)

(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("ix.io" "dpaste.org")))

(provide 'setup-programming)
;;; setup-programming.el ends here
