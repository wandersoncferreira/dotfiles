;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! pinentry)
(package! nov)
(package! uuidgen)
(package! kaocha-runner)
(package! crux)
(package! vlf)

;; apps
(package! pomidor)
(package! annotate)

;; search
(package! ripgrep)
(package! ag)
(package! deadgrep)

;; editor & movement
(package! paredit)
(package! change-inner)
(package! jump-char)
(package! iedit)
(package! unfill)
(package! fix-word)
(package! move-text)
(package! cycle-quotes)

;; window & buffers
(package! rotate)
(package! switch-buffer-functions)
(package! windresize)
(package! popper)

;; network
(package! httprepl)
(package! webpaste)

;; macos
(package! counsel-osx-app)

;; demo emacs buffers
(package! keycast)
(package! gif-screencast)

;; ui
(package! solaire-mode :disable t) ;; makes non-file-visiting buffers darker than the rest of the Emacs' frame
(package! delight)
(package! alect-themes)
(package! sublime-themes)
(package! spacemacs-theme)
(package! color-theme-modern)
(package! treemacs-all-the-icons)

;; completion
(package! ivy-rich :disable t)
(package! ivy-hydra :disable t)

;; dired
(package! diredfl :disable t)

;; vc
(package! bug-reference-github)
(package! magit-gitflow :disable t)
(package! magit-todos :disable t)
(package! github-review :disable t) ;; preference to my own fork

;; lookup
(package! request :disable t)

;; unpin
(unpin! cider)
(unpin! clojure-mode)
(unpin! clj-refactor)
(unpin! parseclj)
(unpin! treemacs)
(unpin! magit forge)
(unpin! lsp-mode
        lsp-treemacs
        lsp-mode
        lsp-ui)

;; pin
(package! map :pin "bb50dba") ;; fix cider bug here https://github.com/clojure-emacs/cider/issues/3029

;; transitive dependencies
(package! deferred) ;; support github-review
(package! a) ;; support github-review
