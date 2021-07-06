;;; ../dotfiles/.doom.d/+extra-typescript.el -*- lexical-binding: t; -*-

(add-hook 'typescript-mode-hook #'format-all-mode)
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
