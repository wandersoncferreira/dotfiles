;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (modeline +light) ; snazzy, Atom-inspired modeline, plus API
       (popup +defaults) ; tame sudden yet inevitable temporary windows
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB

       :editor
       file-templates    ; auto-snippets for empty files
       (format)          ; automated prettiness
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       vc                ; version-control and Emacs, sitting in a tree

       :term
       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       ;;editorconfig        ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;(eval +overlay)     ; run code, run (also, repls)
       gist                ; interacting with github gists
       (lookup +docsets)   ; navigate your code and its documentation
       lsp
       (magit +forge)      ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       cc                  ; C > C++ == 1
       (clojure +lsp)      ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp          ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       (haskell +lsp)      ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       json                ; At least it ain't XML
       (java +lsp)         ; the poster child for carpal tunnel syndrome
       (javascript +lsp)   ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ledger              ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown            ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org
        +roam
        +dragndrop)        ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       plantuml            ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python              ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                  ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +gmail)
       ;;notmuch

       :app
       ;;calendar
       ;;emms
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       (default +bindings))
