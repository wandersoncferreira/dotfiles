;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wand@hey.com"
      byte-compile-warnings '(cl-functions)
      enable-local-variables t)

;; mode alist
(add-to-list 'auto-mode-alist '("\\ledger\\'" . ledger-mode))

(defun bk/load-gpg-file (fname)
  (let* ((fname-gpg (concat fname ".el.gpg"))
         (fpath (concat "~/.doom.d/" fname-gpg)))
    (add-hook 'after-init-hook
              (lambda ()
                (load-file fpath)))))

(when IS-MAC
  (setq alert-default-style 'osx-notifier))

;; scratch buffer
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode
      initial-major-mode 'emacs-lisp-mode)

;; utf8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; defaults
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

;; keybindigns
(load! "+extra-bindings")

;; work
(load! "+work-reifyhealth")
(load! "+work-appsauce")
(bk/load-gpg-file "+work-cisco")
