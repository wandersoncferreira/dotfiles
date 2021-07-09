;;; ../dotfiles/.doom.d/+extra-programming.el -*- lexical-binding: t; -*-

(after! company
  (setq company-idle-delay 0.2))

(after! lsp-mode
  (setq lsp-enable-file-watchers t
        lsp-ui-sideline-show-code-actions nil
        lsp-enable-symbol-highlighting t
        lsp-eldoc-enable-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-idle-delay 0.2
        lsp-headerline-breadcrumb-enable nil)

  (add-to-list 'lsp-file-watch-ignored-directories "classes")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'"))

(advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))
