;;; setup-typescript --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:


;; * Attributes
(setq-default js-indent-level 2)


;; * External Dependencies

(defun bk/setup-tide-mode ()
  "Setup js development."
  (interactive)
  (tide-setup)
  (eldoc-mode -1)
  (electric-pair-mode +1)
  (tide-hl-identifier-mode +1)
  (prettier-js-mode +1)
  (add-node-modules-path))

(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx\\'" . typescript-mode)
  :hook (typescript-mode . bk/setup-tide-mode))

(use-package prettier-js :ensure t)

(use-package add-node-modules-path :ensure t)

(use-package js2-mode
  :ensure t
  :init
  (setq js2-mode-show-parse-errors nil
        js2-missing-semi-one-line-override nil)
  :config
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode +1))))

(use-package js2-refactor
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package tide
  :ensure t
  :diminish tide-mode
  :init
  (setq tide-completion-detailed t
        tide-always-show-documentation t
        tide-server-max-response-length 524288)
  :hook ((js-mode . bk/setup-tide-mode)
         (js2-mode . bk/setup-tide-mode)
         (typescript-mode . bk/setup-tide-mode)))

(provide 'setup-typescript)
;;; setup-typescript.el ends here
