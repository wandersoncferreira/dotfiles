;;; setup-launcher --- LAUNCHER programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package prodigy
  :ensure t
  :bind
  (("C-c s l" . prodigy))
  :config
  (prodigy-define-service
    :name "tempo"
    :command "make"
    :cwd "~/repos/experiments/tempo/"
    :stop-signal 'sigkill
    :tags '(eas-tempo)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "esource-repl"
    :command "make"
    :args '("repl")
    :cwd "~/repos/reifyhealth/esource-service/"
    :stop-signal 'sigkill
    :tags '(esource-repl)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "esource"
    :command "make"
    :args '("run")
    :cwd "~/repos/reifyhealth/esource-service/"
    :stop-signal 'sigkill
    :tags '(esource-run)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "study-sheet"
    :command "yarn"
    :args '("start")
    :cwd "~/repos/reifyhealth/study-sheet/"
    :stop-signal 'sigkill
    :tags '(stdocs)
    :kill-process-buffer-on-stop t)

  (prodigy-define-tag
    :name 'eas-tempo
    :ready-message ".*Build completed.*")

  (prodigy-define-tag
    :name 'esource-repl
    :ready-message "nREPL started!")

  (prodigy-define-tag
    :name 'stdocs
    :ready-message "Starting the development server...")

  (prodigy-define-tag
    :name 'esource-run
    :ready-message "WARNING: .*"))

(provide 'setup-launcher)
;;; setup-launcher.el ends here
