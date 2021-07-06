;;; ../dotfiles/.doom.d/+extra-appearance.el -*- lexical-binding: t; -*-

;; delete selection
(delete-selection-mode +1)

;; fullscreen from beginning
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

;; change highlight colors
(set-face-attribute 'lazy-highlight nil :background "khaki1")
(set-face-attribute 'isearch nil :background "khaki1")
(set-face-attribute 'region nil :background "khaki1")
(set-face-attribute 'default nil :height 100)
