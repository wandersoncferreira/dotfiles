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

;; defaults
(load! "+extra-appearance")
(load! "+extra-auth")
(load! "+extra-org")
(load! "+extra-window")
(load! "+extra-vc")
(load! "+extra-spell")

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
