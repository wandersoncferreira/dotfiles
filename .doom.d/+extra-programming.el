;;; ../dotfiles/.doom.d/+extra-programming.el -*- lexical-binding: t; -*-

(set-company-backend! 'prog-mode nil)
(set-company-backend! 'prog-mode
  '(:separate company-capf company-files company-dabbrev-code company-yasnippet))

(setq company-idle-delay 0.1)

(after! lsp-mode
  (setq lsp-enable-file-watchers t
        lsp-ui-sideline-show-code-actions nil
        lsp-enable-symbol-highlighting t
        lsp-eldoc-enable-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-idle-delay 0.1
        lsp-headerline-breadcrumb-enable nil))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "classes")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'"))

(add-hook! 'lsp-before-open-hook
  (remove-hook! 'lsp-completion-mode-hook #'+lsp-init-company-backends-h))

(advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))

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
