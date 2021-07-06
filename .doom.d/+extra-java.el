;;; ../dotfiles/.doom.d/+extra-java.el -*- lexical-binding: t; -*-

(after! lsp-java
  (setq lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-save-actions-organize-imports t)

  (add-hook! 'java-mode-hook (electric-pair-mode +1))
  (add-hook! 'java-mode-hook (subword-mode +1)))


(after! cc-mode
  (remove-hook 'java-mode-hook #'rainbow-delimiters-mode))
