;;; ../dotfiles/.doom.d/+extra-auth.el -*- lexical-binding: t; -*-

(require 'epa-file)
(require 'org-crypt)

(setq auth-source-debug t
      auth-sources '((:source "~/.secrets/authinfo.gpg"))
      epg-gpg-program "gpg"
      org-tags-exclude-from-inheritance (quote ("crypt"))
      )

(epa-file-enable)
(org-crypt-use-before-save-magic)

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
