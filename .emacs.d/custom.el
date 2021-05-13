(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
     (cider-ns-refresh-after-fn . "user/reset")
     (cider-known-endpoints
      ("localhost" "8777"))
     (cider-docker-translations
      ("/app/src" . "/home/wand/secland/src")
      ("/app/test" . "/home/wand/secland/test"))
     (cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-ns-refresh-after-fn . "reloaded.repl/resume")
     (cider-ns-refresh-before-fn . "reloaded.repl/suspend")))
 '(session-use-package t nil (session))
 '(wakatime-cli-path "/home/wanderson/.nix-profile/bin/wakatime")
 '(wakatime-python-bin nil)
 '(warning-suppress-types '((magit-todos) (comp))))
