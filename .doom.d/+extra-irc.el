;;; ../code/dotfiles/.doom.d/+extra-irc.el -*- lexical-binding: t; -*-

(require 'erc)
(require 'erc-track)

(setq erc-server "irc.libera.chat"
      erc-nick "bartuka"
      erc-user-full-name "Wanderson Ferreira"
      erc-track-shorten-start 8
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-prompt-for-password nil)

(erc-autojoin-mode +1)
(setq erc-autojoin-channels-alist
      '(("irc.libera.chat" "#emacs" "#systemcrafters" "#code-review-emacs"))
      erc-autojoin-timing :ident
      erc-autojoin-delay 30
      erc-join-buffer 'bury)

(erc-track-mode +1)
(setq erc-keywords '("code-review" "emacs" "clojure")
      erc-track-exclude-server-buffer t
      erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE")
      erc-track-use-faces t
      erc-track-faces-priority-list
      '(erc-current-nick-face
        erc-keyword-face
        erc-direct-msg-face)
      erc-track-priority-faces-only 'all)

(defun twitch-start-irc ()
  "Connect to Twitch IRC.
Get an oauth token from this website https://twitchapps.com/tmi/."
  (interactive)
  (let* ((host "irc.chat.twitch.tv")
         (user "bartuka_")
         (pwd (auth-source-pick-first-password
               :host host
               :user user)))
    (erc-tls :server host
             :port 6697
             :nick user
             :password pwd)))
