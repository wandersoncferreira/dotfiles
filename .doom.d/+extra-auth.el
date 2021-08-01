;;; ../dotfiles/.doom.d/+extra-auth.el -*- lexical-binding: t; -*-

(setq auth-source-debug t
      auth-sources '((:source "~/.secrets/authinfo.gpg"))
      epg-gpg-program "gpg")

(defun bk/bitwarden ()
  "Get bitwarden."
  (interactive)
  (kill-new (auth-source-pick-first-password
             :host "bitwarden.app"
             :user "bartuka")))
