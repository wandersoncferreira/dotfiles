;;; ../dotfiles/.doom.d/+extra-apps.el -*- lexical-binding: t; -*-

;; view large files
(use-package! vlf
  :config
  (require 'vlf-setup)
  (custom-set-variables
   '(vlf-application 'dont-ask)))

;; screencast
(use-package! gif-screencast
  :config
  (setq gif-screencast-args '("-x")
        gif-screencast-cropping-program "mogrify"
        gif-screencast-capture-format "ppm"))

(use-package! plantuml-mode
  :config
  (setq plantuml-jar-path "~/dotfiles/plantuml.jar"))

;;; keyfreq
(use-package! keyfreq
  :init
  (setq keyfreq-excluded-commands
        '(self-insert-command))
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

;;; mouse
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))
