;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; mode alist
(add-to-list 'auto-mode-alist '("\\ledger\\'" . ledger-mode))

;; defaults
(load! "+extra-defaults")
(load! "+extra-appearance")
(load! "+extra-auth")
(load! "+extra-org")
(load! "+extra-window")
(load! "+extra-vc")

;; langs
(load! "+extra-programming")
(load! "+extra-clojure")
(load! "+extra-java")
(load! "+extra-typescript")
(load! "+extra-elisp")

;; apps
(load! "+extra-ledger")

;; keybindigns
(load! "+extra-bindings")

;; work
(load! "+work-reifyhealth")
(load! "+work-appsauce")
