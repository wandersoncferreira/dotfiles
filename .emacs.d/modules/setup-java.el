;;; setup-java --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:


;;*  External Dependencies

(use-package lsp-mode
  :ensure t
  :hook ((java-mode . #'lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-intelephense-multi-root nil
        lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-completion-provider :capf
        lsp-idle-delay 0.500))

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug))
  :hook ((dap-mode . dap-ui-mode)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-delay 1.5
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-width 100))

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp))

(defun bk/setup-java ()
  "Setup JAVA."
  (electric-pair-mode +1))

(add-hook 'java-mode-hook 'bk/setup-java)

(provide 'setup-java)
;;; setup-java.el ends here
