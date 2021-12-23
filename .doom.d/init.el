;;; init.el -*- lexical-binding: t; -*-

(setq +literate-config-file "~/.doom.d/README.org")

(doom!
 :input

 :completion
 company
 (vertico +icons)

 :ui
 doom
 (emoji +github)
 hl-todo
 (popup
  +all
  +defaults)
 (treemacs +lsp)
 vc-gutter
 (window-select +numbers)
 workspaces

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
 (eval +overlay)
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
 (clojure
  +lsp)
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
  +pretty
  +dragndrop)
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
