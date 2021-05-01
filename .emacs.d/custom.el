(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   '(perspective envrc pdf-tools editorconfig helpful zoom-frm diff-hl diff-hl-mode whitespace-cleanup-mode whitespace-cleanup zprint-mode windresize which-key vlf uuidgen use-package toggle-test tide sql-indent smex rg rainbow-delimiters quickrun projectile prettier-js popup pinentry org-roam-server org-download nixpkgs-fmt nix-sandbox nix-mode nix-buffer move-text moody magit-todos lsp-ui lsp-java ledger-mode langtool kaocha-runner jump-char java-snippets ido-completing-read+ ido-at-point google-translate google-this gitignore-templates gitignore-mode gitconfig-mode git-timemachine gist forge flycheck-clj-kondo flx-ido fix-word edit-indirect dockerfile-mode docker-compose-mode docker diminish company-nixos-options clojure-snippets clj-refactor change-inner buffer-move browse-kill-ring browse-at-remote bicycle aggressive-indent add-node-modules-path))
 '(safe-local-variable-values
   '((tgt-projects
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
     (cider)
     (cider-ns-refresh-after-fn . "user/reset")
     (cider-known-endpoints
      ("localhost" "8777"))
     (cider-docker-translations
      ("/app/src" . "/home/wand/secland/src")
      ("/app/test" . "/home/wand/secland/test"))
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-ns-refresh-after-fn . "reloaded.repl/resume")
     (cider-ns-refresh-before-fn . "reloaded.repl/suspend")))
 '(warning-suppress-types '((magit-todos) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(lsp-lsp-flycheck-error-unnecessary-face ((t (:underline (:color "Red1" :style wave)))) t)
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:underline (:color "dark orange" :style wave)))) t))
