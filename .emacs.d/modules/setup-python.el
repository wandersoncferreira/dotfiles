;;; setup-python --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(defun bk/elpy-setup ()
  "Setup python mode."
  (pyvenv-activate "~/miniconda3")
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(use-package elpy
  :ensure t
  :config
  (add-hook 'python-mode-hook #'elpy-enable)
  (add-hook 'python-mode-hook #'bk/elpy-setup))

(use-package py-autopep8
  :ensure t
  :after elpy
  :init
  (setq py-autopep8-options '("--max-line-length=100"))
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


(provide 'setup-python)
;;; setup-python.el ends here
