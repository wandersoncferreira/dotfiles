;;; captalys --- Code related to work at Captalys
;;
;;; Commentary:
;;
;;; Code:


;; * Functions

(defun bk/dockerfile-add-build-args ()
  "Add env variables to your docker build."
  (interactive)
  (let* ((vars (read-from-minibuffer "sequence of <envName>=<envValue>: "))
         (split-vars (split-string vars " ")))
    (setq dockerfile-build-args nil)
    (dolist (v split-vars)
      (add-to-list 'dockerfile-build-args v))
    (setq docker-build-history-args vars)))


(defun bk/docker-compose-custom-envs ()
  "Add usual env variables to Emacs environment."
  (interactive)
  (let* ((idu (shell-command-to-string "id -u"))
         (idg (shell-command-to-string "id -g"))
         (uid (string-join (vector (string-trim idu) ":" (string-trim idg)))))
    (setenv "WEBSERVER_PORT" "3000")
    (setenv "CURRENT_UID" uid)
    (message "setenv WEBSERVER_PORT=3000 CURRENT_UID=$(id -u):$(id -g) done!")))

(defun bk/docker-cleanup-buffers ()
  "Delete all the docker buffers created."
  (interactive)
  (kill-matching-buffers "docker" nil t))

(defun bk/generate-password ()
  "Generate a 16-digit password."
  (interactive)
  (let* ((sym "!@#$%^&*()_+-=[]{}|")
         (i (% (abs (random)) (length sym)))
         (beg (substring sym i (1+ i))))
    (kill-new
     (format "%s%s" beg
             (string-trim
              (shell-command-to-string
               " openssl rand -base64 32 | tr -d /=+ | cut -c -16"))))))

;; * External Dependencies

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile" . dockerfile-mode)
  :config
  (put 'dockerfile-image-name 'safe-local-variable-p #'stringp))

(use-package docker-compose-mode :ensure t)

(use-package docker
  :ensure t
  :bind
  ("C-c d d" . docker))

(provide 'captalys)
;;; captalys.el ends here
