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
(package! delight)
(package! ripgrep)
(package! ag)
(package! bug-reference-github)

;; support github-review
(package! deferred)
(package! a)

;; themes
(package! alect-themes)
(package! organic-green-theme)
(package! sublime-themes)
(package! spacemacs-theme)
(package! color-theme-modern)

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
(package! magit-gitflow :disable t)
(package! magit-todos :disable t)
;; preference to my own fork
(package! github-review :disable t)

;; lookup
(package! request :disable t)

;;; unpin
(unpin! lsp-mode)
(unpin! cider)
(unpin! clojure-mode)
(unpin! magit magit-todos forge)

;; fix cider bug here https://github.com/clojure-emacs/cider/issues/3029
(package! map :pin "bb50dba")
