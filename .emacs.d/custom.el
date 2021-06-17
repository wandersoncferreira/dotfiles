(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(safe-local-variable-values
   '((sql-postgres-login-params
      (user :default "esource")
      (database :default "esource")
      (server :default "localhost")
      (port :default 5432))
     (tgt-projects
      ((:root-dir "/home/wanderson/repos/reifyhealth/esource-service")
       (:src-dirs "src")
       (:test-dirs "test")
       (:test-suffixes "_test")))
     (cider-ns-refresh-before-fn . "user/stop")
     (cider-ns-refresh-after-fn . "user/reset")
     (cider-known-endpoints
      ("localhost" "12344"))))
 '(warning-suppress-types '((magit-todos) (comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
