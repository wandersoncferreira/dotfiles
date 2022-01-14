;;; init.el -*- lexical-binding: t; -*-

(setq +literate-config-file "~/.doom.d/README.org")

;;; remove the automatic tangle functionality
(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(doom!
 :input

 :completion
 company
 vertico

 :ui
 doom
 (modeline +light)
 (emoji +github)
 hl-todo
 (popup
  +all
  +defaults)
 vc-gutter
 (window-select +numbers)

 :editor
 file-templates
 format
 multiple-cursors
 rotate-text
 snippets

 :emacs
 dired
 electric
 vc

 :term
 eshell

 :checkers
 syntax
 (spell
  +aspell
  +everywhere)
 grammar

 :tools
 eval
 gist
 (lookup
  +dictionary
  +docsets)
 lsp
 (magit +forge)
 make
 pdf

 :os
 (:if IS-MAC macos)

 :lang
 cc
 clojure
 emacs-lisp
 (haskell +lsp)
 json
 (java +lsp)
 (javascript +lsp)
 ledger
 markdown
 (org
  +roam2
  +present
  +dragndrop
  +hugo)
 plantuml
 rest
 sh
 yaml

 :email

 :app
 irc
 (rss +org)

 :config
 literate
 (default +bindings))
