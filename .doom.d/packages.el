;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! paredit)
(package! change-inner)
(package! jump-char)
(package! fix-word)
(package! pinentry)
(package! move-text)
(package! nov)
(package! uuidgen)
(package! windresize)
(package! webpaste)
(package! org-roam-server)
(package! kaocha-runner)
(package! crux)
(package! pomidor)

;; themes
(package! alect-themes)

;;; disable packages

;; ivy
(package! ivy-rich :disable t)
(package! ivy-hydra :disable t)

;; company
(package! company-dict :disable t)

;; dired
(package! diredfl :disable t)
(package! dired-rsync :disable t)

;; vc
(package! diff-hl :disable t)
(package! magit-gitflow :disable t)

;; lookup
(package! request :disable t)

;; elisp
(package! buttercup :disable t)
(package! macrostep :disable t)
(package! overseer :disable t)

;;; unpin
(unpin! lsp-mode)
(unpin! cider)
(unpin! clojure-mode)
