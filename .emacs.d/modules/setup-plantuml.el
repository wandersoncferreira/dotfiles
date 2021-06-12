;;; setup-plantuml --- PLANTUML programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :config
  (setq org-plantuml-jar-path "~/.emacs.d/bin/plantuml.jar"
        plantuml-default-exec-mode 'jar)
  (require 'ob-plantuml))

(use-package flycheck-plantuml
  :ensure t
  :after flycheck
  :config
  (flycheck-plantuml-setup))

(provide 'setup-plantuml)
;;; setup-plantuml.el ends here
