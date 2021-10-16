;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; patches
(load! "+patch-doom")
(load! "+patch-deadgrep")
(load! "+patch-counsel")
(load! "+patch-cider")
(load! "+patch-persp")
(load! "+patch-ivy-posframe")

;; defaults
(load! "+extra-defaults")
(load! "+extra-appearance")
(load! "+extra-auth")
(load! "+extra-org")
(load! "+extra-window")
(load! "+extra-vc")
(load! "+extra-spell")
(load! "+extra-eshell")
(load! "+extra-workspaces")

;; os
(load! "+extra-macos")

;; langs
(load! "+extra-programming")
(load! "+extra-clojure")
(load! "+extra-java")
(load! "+extra-typescript")
(load! "+extra-elisp")

;; apps
(load! "+extra-ledger")
(load! "+extra-apps")

;; keybindigns
(load! "+extra-bindings")

;; work
(load! "+work-reifyhealth")
(load! "+work-appsauce")
(load! "+work-cisco" nil nil t)
