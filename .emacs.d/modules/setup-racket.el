;;; setup-racket --- Racket programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package racket-mode
  :ensure t
  :bind (:map racket-mode-map
              ("C-c C-k" . racket-run))
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

(provide 'setup-racket)
;;; setup-racket.el ends here
