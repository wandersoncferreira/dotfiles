;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wand@hey.com"

      byte-compile-warnings '(cl-functions)
      scroll-margin 2
      enable-local-variables t
      load-prefer-newer t
      show-help-function nil
      read-process-output-max (* 1024 1024)
      dired-listing-switches "-ahl -v"

      projectile-enable-caching nil
      projectile-project-search-path '("~/code"))

;; use single abbrev-table for multiple modes
(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))

;; view large files
(use-package! vlf
  :config
  (require 'vlf-setup)
  (custom-set-variables
   '(vlf-application 'dont-ask)))

;; screencast
(after! gif-screencast
  (setq gif-screencast-args '("-x")
        gif-screencast-cropping-program "mogrify"
        gif-screencast-capture-format "ppm"))

;; mode alist
(add-to-list 'auto-mode-alist '("\\ledger\\'" . ledger-mode))

(defun bk/load-gpg-file (fname)
  (let* ((fname-gpg (concat fname ".el.gpg"))
         (fpath (concat "~/.doom.d/" fname-gpg)))
    (add-hook 'after-init-hook
              (lambda ()
                (load-file fpath)))))

;; scratch buffer
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode
      initial-major-mode 'emacs-lisp-mode)

;; enable command
(put 'narrow-to-region 'disabled nil)

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

;; patches
(load! "+patch-deadgrep")
(load! "+patch-counsel")
(load! "+patch-cider")
(load! "+patch-persp")
(load! "+patch-ivy-posframe")
