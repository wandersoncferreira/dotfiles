;;; ../dotfiles/.doom.d/+extra-programming.el -*- lexical-binding: t; -*-

(use-package! company
  :init
  (setq company-idle-delay 0.1
        company-show-quick-access t
        company-icon-size 20)
  :config
  (set-company-backend! 'prog-mode nil)
  (set-company-backend! 'prog-mode
    '(:separate company-capf company-files company-dabbrev-code company-yasnippet)))

(use-package! lsp-mode
  :init
  (setq lsp-enable-file-watchers t
        lsp-enable-symbol-highlighting t
        lsp-eldoc-enable-hover t
        lsp-lens-enable t
        lsp-idle-delay 0.1
        lsp-headerline-breadcrumb-enable nil)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "classes")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'")

  (add-hook! 'lsp-before-open-hook
    (remove-hook! 'lsp-completion-mode-hook #'+lsp-init-company-backends-h))

  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers))))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-max-width 60
        lsp-ui-doc-position 'top
        lsp-ui-doc-delay 0.2
        lsp-ui-peek-list-width 60
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

;; when you hit Ctrl+;, all occurrences of the symbol under the cursor (or
;; current selection) are highlighted, and any changes you make on one of them
;; will be automatically applied to all others.
(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "saddle brown")
  :bind
  ("C-;" . iedit-mode))

;; incredible small utility by Magnars
;; symbol-focus allow us to easily edit pieces of code in isolation
(use-package! symbol-focus
  :load-path "~/.doom.d/sources/symbol-focus"
  :config
  (add-hook 'prog-mode-hook #'symbol-focus-mode))

(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package! treemacs-all-the-icons
  :after treemacs)

(use-package! hungry-delete
  :delight hungry-delete-mode
  :config
  (add-hook 'prog-mode-hook 'hungry-delete-mode))
