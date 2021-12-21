;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! pinentry)
(package! nov)
(package! uuidgen)
(package! kaocha-runner)
(package! crux)
(package! vlf)
(package! inflections)

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
(package! hungry-delete)

;; window & buffers
(package! rotate)
(package! switch-buffer-functions)
(package! windresize)

;; network
(package! httprepl)
(package! webpaste)

;; macos
(package! counsel-osx-app)

;; demo emacs buffers
(package! keycast)
(package! gif-screencast)

;; ui
(package! delight)
(package! alect-themes)
(package! sublime-themes)
(package! spacemacs-theme)
(package! color-theme-modern)
(package! treemacs-all-the-icons)
(package! ivy-posframe)

;; dired
(package! diredfl :disable t)
(package! dired-imenu)

;; vc
(package! bug-reference-github)
(package! magit-gitflow :disable t)
(package! code-review :disable t)

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

(package! a)

;; pin
(package! map :pin "bb50dba") ;; fix cider bug here https://github.com/clojure-emacs/cider/issues/3029

;;; org roam
(unpin! org-roam)
(package! org-roam-ui)

;; mode
(package! graphql-mode)

;; matrix
(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))

(package! ement
  :recipe (:host github :repo "alphapapa/ement.el" :files ("*.el")))

;;; keyfreq
(package! keyfreq)

;;; disable mouse
(package! disable-mouse)
