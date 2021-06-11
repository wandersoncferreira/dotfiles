;;; setup-presentation --- Tools to improve presentation
;;
;;; Commentary:
;;
;;; Code:

;; * Functions

(require 'setup-appearance)

(defun bk/presentation-theme ()
  "Presentation theme."
  (interactive)
  (bk/set-ibm-font 120)
  (disable-theme 'default-black)
  (bk/appearance))


;; * External Dependencies

(use-package keycast
  :ensure t)

(use-package gif-screencast
  :ensure nil
  :load-path "~/.emacs.d/lisps/gif-screencast.el"
  :config
  (setq gif-screencast-program "maim"
        gif-screencast-args '("--quality" "1")
        gifgif-screencast-want-optimized t
        gif-screencast-output-directory "~/Videos/emacs/"
        gif-screencast-screenshot-directory "~/.emacs.d/screenshots"))


(provide 'setup-presentation)
;;; setup-presentation.el ends here
