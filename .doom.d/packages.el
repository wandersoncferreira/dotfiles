;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! paredit)
(package! change-inner)
(package! jump-char)
(package! pinentry)
(package! fix-word)
(package! move-text)
(package! nov)
(package! toggle-test)
(package! uuidgen)
(package! windresize)
(package! webpaste)
(package! org-roam-server)
(package! kaocha-runner)
(package! unison-mode)

;;; disable packages
(package! ivy-rich :disable t)
(package! diredfl :disable t)
(package! dired-rsync :disable t)
(package! magit-gitflow :disable t)


;;; unpin
(unpin! lsp-mode)
(unpin! cider)
(unpin! clojure-mode)
