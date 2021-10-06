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
(package! delight)
(package! pomidor)
(package! ripgrep)
(package! ag)
(package! bug-reference-github)

;; support github-review
(package! deferred)
(package! a)

;; themes
(package! alect-themes)
(package! sublime-themes)
(package! spacemacs-theme)
(package! color-theme-modern)

;;; disable packages

;; ui

;; makes non-file-visiting buffers darker than the rest of the Emacs' frame. Disabled.
(package! solaire-mode :disable t)

;; ivy
(package! ivy-rich :disable t)
(package! ivy-hydra :disable t)

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
(unpin! cider)
(unpin! clojure-mode)
(unpin! clj-refactor)
(unpin! magit magit-todos forge)

;; fix cider bug here https://github.com/clojure-emacs/cider/issues/3029
(package! map :pin "bb50dba")
