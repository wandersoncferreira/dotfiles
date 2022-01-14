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
(package! hackernews)

;; search
(package! ripgrep)
(package! ag)
(package! deadgrep)
(package! engine-mode)

;; editor & movement
(package! paredit)
(package! change-inner)
(package! jump-char)
(package! iedit)
(package! unfill)
(package! fix-word)
(package! move-text)
(package! cycle-quotes)
(package! digit-groups)
(package! unicode-troll-stopper)

;; window & buffers
(package! rotate)
(package! switch-buffer-functions)
(package! windresize)
(package! zygospore)

;; network
(package! httprepl)
(package! webpaste)

;; demo emacs buffers
(package! keycast)
(package! gif-screencast)

;; ui
(package! alect-themes)
(package! sublime-themes)
(package! spacemacs-theme)
(package! color-theme-modern)
(package! theme-changer)
(package! kaolin-themes)
(package! treemacs-all-the-icons)
(package! svg-tag-mode)

;; dired
 (package! dired-imenu)

;; vc
(package! bug-reference-github)
(package! magit-gitflow :disable t)

;; lookup
(package! request :disable t)

;; clojure
(unpin! cider)
(unpin! clojure-mode)
(unpin! clj-refactor)
(package! clojure-snippets)

(unpin! parseclj)
(unpin! treemacs)
(unpin! magit forge)
(unpin! lsp-mode
        lsp-treemacs
        lsp-mode
        lsp-ui)

;; code review development
(package! code-review
  :recipe (:local-repo "~/code/code-review"
           :build (:not compile)))

(package! a)
(package! deferred)

;; pin
(package! map :pin "bb50dba") ;; fix cider bug here https://github.com/clojure-emacs/cider/issues/3029

;;; org
(unpin! org-roam)
(package! org-roam-ui)
(package! mixed-pitch)
(package! org-fragtog)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-super-agenda)
(package! citeproc-org)

;; mode
(package! graphql-mode)

;; matrix
(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))

(package! ement
  :recipe (:host github :repo "alphapapa/ement.el" :files ("*.el")))

;;; keyfreq
(package! keyfreq)

;;; spell
(package! lsp-grammarly)
(package! keytar)
(package! define-it)

;;; finance
(package! dklrt)

;;; completion
(unpin! vertico)
