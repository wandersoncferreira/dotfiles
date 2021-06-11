;;; setup-eshell --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:

(require 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")))

;; * Functions

(defun eshell-clear-buffer ()
  "Clear the terminal buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; * External Dependencies

(use-package eshell-toggle
  :ensure t
  :bind ("C-x e" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
