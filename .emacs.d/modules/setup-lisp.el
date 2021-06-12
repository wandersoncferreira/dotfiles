;;; setup-lisp --- Lisp programming language
;;
;;; Commentary:
;;
;;; Code:

;; * External Dependencies

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'git-commit-mode-hook 'enable-paredit-mode)
  (add-hook 'racket-mode-hook 'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook 'enable-paredit-mode)
  (with-eval-after-load "eldoc"
    (eldoc-add-command #'paredit-backward-delete #'paredit-close-round)))

(provide 'setup-lisp)
;;; setup-lisp.el ends here
