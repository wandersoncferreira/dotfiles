;;; ../dotfiles/.doom.d/+extra-auth.el -*- lexical-binding: t; -*-

(require 'epa)

(setq auth-source-debug t
      auth-sources '((:source "~/.secrets/authinfo.gpg"))
      epg-gpg-program "gpg"
      epa-file-cache-passphrase-for-symmetric-encryption t
      epa-pinentry-mode 'ask
      epg-pinentry-mode 'ask)

(pinentry-start)

(defun bk/bitwarden ()
  "Get bitwarden."
  (interactive)
  (kill-new (auth-source-pick-first-password
             :host "bitwarden.app"
             :user "bartuka")))
