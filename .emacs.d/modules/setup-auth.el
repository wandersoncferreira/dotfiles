;;; setup-auth --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:

(require 'epa)
(require 'epg-config)

;; * Attributes

(setq auth-source-debug t
      auth-sources '((:source "~/.secrets/authinfo.gpg")))

(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-pinentry-mode 'loopback)

;; * Functions

(defun bk/bitwarden ()
  "Get bitwarden."
  (interactive)
  (kill-new (auth-source-pick-first-password
             :host "bitwarden.app"
             :user "bartuka")))

(defun my-gpg ()
  "My gpg."
  (interactive)
  (kill-new
   (with-temp-buffer
     (insert-file-contents "~/.secrets/pwd/gpg.txt")
     (buffer-string))))

;; * External Dependencies

(use-package pinentry
  :ensure t
  :config
  (setq epg-gpg-program "gpg")
  (when (not (process-live-p pinentry--server-process))
    (pinentry-start)))

(provide 'setup-auth)
;;; setup-auth.el ends here
