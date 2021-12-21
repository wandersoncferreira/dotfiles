;;; ../dotfiles/.doom.d/+extra-elisp.el -*- lexical-binding: t; -*-

(after! elisp-mode
  (remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package! paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))

(use-package! buttercup
  :config
  (define-advice buttercup--run-suites (:around (orig-fun &rest args))
    "Keep the cursor at-point after running test suite with buttercup."
    (setq save-point (point))
    (unwind-protect
        (apply orig-fun args)
      (goto-char save-point))))
