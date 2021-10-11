;;; ../dotfiles/.doom.d/+extra-auth.el -*- lexical-binding: t; -*-

(require 'epa-file)
(require 'org-crypt)

(setq epg-gpg-program "gpg"
      org-tags-exclude-from-inheritance (quote ("crypt"))
      password-cache-expiry nil)

(after! auth-source
  (setq auth-sources (nreverse auth-sources)
        auth-source-cache-expiry nil
        auth-source-debug t))

(after! epa
  (set 'epg-pinentry-mode nil)
  (setq epa-file-encrypt-to '("wand@hey.com")))

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
