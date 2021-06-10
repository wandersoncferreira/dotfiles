(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   '(("music" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:music")
     ("netcash" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -R -X R$ --current bal ^assets:bank ^assets:crypto liabilities:card")
     ("sports" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:sports")
     ("doctor" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:doctor")
     ("apartamento-mae" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ -S date --current -w reg ^liabilities:apartment:mother")
     ("apartamento-misce" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ -S date --current -w reg ^liabilities:apartment:misce")
     ("eas-profit" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --invert --current bal ^expenses:eval ^income:eval")
     ("food" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:food")
     ("donation" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:donation")
     ("apartamento-morumbi" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:house")
     ("creta" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:car:creta ^equity:car:creta")
     ("networth" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^assets:bank liabilities equity:apartment")
     ("spent-vs-earned" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger bal -X BRL --period=\"last 4 weeks\" ^Expenses ^Income --invert -S amount")
     ("budget" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^assets:bank:checking:budget liabilities:card")
     ("taxes" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -R -X R$ --current bal ^expenses:taxes")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")) nil nil "Customized with use-package ledger-mode")
 '(package-selected-packages
   '(dot-mode yasnippet-snippets lsp-ui lsp-java dap-java dap-mode lsp-mode zenburn-theme webpaste all-the-icons alert telega editorconfig crux bufler git-link fish-completion eshell-syntax-highlighting eshell-toggle helm-spotify-plus shell-pop gif-screencast keycast nov flycheck-plantuml sqlformat edit-indirect py-autopep8 elpy flycheck-clj-kondo doom-modeline doom-themes olivetti olivetti-mode shackle rainbow-blocks js2-refactor js2-mode browse-kill-ring disable-mouse rainbow-mode ido-vertical-mode vertico elfeed org-roam popup typescript-mode clojure-mode paredit yasnippet flycheck magit multiple-cursors expand-region racket-mode flycheck-pos-tip flycheck-projectile modus-operandi-theme rainbow-delimiters zygospore zprint-mode writeroom-mode windresize whitespace-cleanup-mode which-key wakatime-mode vlf uuidgen use-package try toggle-test tide sql-indent smex session rg restclient quickrun projectile prodigy prettier-js plantuml-mode pinentry perspective pdf-tools org-roam-server org-download nix-mode move-text monokai-theme magit-todos ledger-mode langtool kaocha-runner jump-char ido-completing-read+ ido-at-point hledger-mode goto-chg google-this gitignore-templates gitignore-mode gitconfig-mode git-timemachine gist forge flyspell-correct flymd fix-word find-file-in-project envrc elfeed-org dtrt-indent dockerfile-mode docker-compose-mode docker diminish clojure-snippets clj-refactor change-inner buffer-move browse-at-remote bm bicycle ag add-node-modules-path))
 '(safe-local-variable-values
   '((writeroom-mode-line . t)
     (writeroom-fullscreen-effect . maximized)
     (writeroom-restore-window-config . t)
     (writeroom-mode-line t)
     (writeroom-extra-line-spacing . t)
     (eval progn
           (turn-off-auto-fill)
           (text-scale-set 1)
           (writeroom-mode))
     (visual-fill-column-width . 80)
     (eval progn
           (turn-off-auto-fill)
           (text-scale-set 1))
     (tgt-projects
      ((:root-dir "/home/wanderson/projects/atvi-broadcast-stats-portico")
       (:src-dirs "src")
       (:test-dirs "test")
       (:test-suffixes "_test")))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/eas/thanos")
       (:src-dirs "src/clj")
       (:test-dirs "test/clj")
       (:test-suffixes "_test")))
     (cider-known-endpoints
      ("localhost" "8772"))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/eas/thanos/agamotto")
       (:src-dirs "src/clj")
       (:test-dirs "test/clj")
       (:test-suffixes "_test")))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/reifyhealth/esource-service")
       (:src-dirs "src")
       (:test-dirs "test")
       (:test-suffixes "_test")))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/reifyhealth/esource-service")
       (:src-dirs "src/clj")
       (:test-dirs "test/clj")
       (:test-suffixes "_test")))
     (cider-known-endpoints
      ("localhost" "12344"))
     (tgt-projects
      ((:root-dir "/home/wand/platform/seu-barriga")
       (:src-dirs "src")
       (:test-dirs "test")
       (:test-suffixes "_test")))
     (cider-docker-translations
      ("/app/src" . "/home/wand/platform/seu-barriga/src")
      ("/app/test" . "/home/wand/platform/seu-barriga/test"))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/experiments/tempo")
       (:src-dirs "src/clj")
       (:test-dirs "test/clj")
       (:test-suffixes "_test")))
     (cider-known-endpoints quote
                            (("localhost" "8777")))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/experiments/tempo")
       (:src-dirs "src/clj")
       (:test-dirs "test")
       (:test-suffixes "_test")))
     (cider-ns-refresh-before-fn . "user/stop")
     (cider-ns-refresh-after-fn . "user/reset")
     (cider-known-endpoints
      ("localhost" "8777"))
     (cider-docker-translations
      ("/app/src" . "/home/wand/secland/src")
      ("/app/test" . "/home/wand/secland/test"))
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-ns-refresh-after-fn . "reloaded.repl/resume")
     (cider-ns-refresh-before-fn . "reloaded.repl/suspend")))
 '(session-use-package t)
 '(wakatime-cli-path "/home/wanderson/.nix-profile/bin/wakatime")
 '(wakatime-python-bin nil)
 '(warning-suppress-types '((magit-todos) (comp))))
