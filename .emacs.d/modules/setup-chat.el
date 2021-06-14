;;; setup-chat --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:


;; * Functions

(defun erc-sound-if-not-server (match-type nickuserhost msg)
  "ERC sound alert based on MATCH-TYPE and NICKUSERHOST and MSG."
  (unless (or
           (string-match "Serv" nickuserhost)
           (string-match nickuserhost (erc-current-nick))
           (string-match "Server" nickuserhost))
    (when (string= match-type "current-nick")
      (start-process-shell-command "lolsound" nil "mpv ~/.emacs.d/sounds/icq-message.wav"))

    (message
     (format "[%s|<%s:%s> %s]"
             (format-time-string "%Hh%M" (date-to-time (current-time-string)))
             (subseq nickuserhost 0 (string-match "!" nickuserhost))
             (or (erc-default-target) "")
             (subseq msg 0 (- (length msg) 1))
             ;; (if (eq (string-match (erc-current-nick) msg) 0)
             ;;           (subseq msg (+ 1 (length (erc-current-nick))) 40)
             ;;           msg
             ;;           )
             )
     ;; Show msg for 20s
     (run-with-timer 20 nil
                     (lambda ()
                       (message nil)))
     )))

(defun bk/nicklist-toggle ()
  "Function to toggle the nicklist in ERC mode."
  (interactive)
  (let ((nicklist-buffer-name (format " *%s-nicklist*" (buffer-name))))
    (if (get-buffer nicklist-buffer-name)
        (kill-buffer nicklist-buffer-name)
      (erc-nicklist))))

(defun bk/erc-start ()
  "Start ERC mode."
  (interactive)
  (if (get-buffer "irc.libera.chat:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.libera.chat" :port 6667 :nick "bartuka"))))

;; * Telegram

(use-package alert :ensure t)

(use-package all-the-icons :ensure t)

(use-package company :ensure t)

(use-package telega
  :ensure t
  :commands telega
  :init
  (setq telega-animation-play-inline nil
        telega-chat-mode-line-format
        '((:eval
           (telega-chatbuf-mode-line-unread))
          (:eval
           (telega-chatbuf-mode-line-marked))
          (:eval
           (telega-chatbuf-mode-line-members nil))
          (:eval
           (telega-chatbuf-mode-line-pinned-msg 20))))
  :config
  (eval-after-load 'alert
    '(add-to-list 'alert-user-configuration
                  '(((:mode . "telega-chat-mode"))
                    log nil)))

  (eval-after-load 'alert
    '(add-to-list 'alert-user-configuration
                  '(((:message . "@bartuka\\|Wanderson")
                     (:mode . "telega-chat-mode"))
                    libnotify nil)))
  (add-hook 'telega-chat-mode-hook 'company-mode)
  (require 'telega-alert)
  (telega-alert-mode t)

  (add-hook 'telega-load-hook 'telega-mode-line-mode)

  (require 'telega-dired-dwim))

;; * Attributes

(use-package erc
  :commands erc
  :init
  (setq erc-server "irc.libera.chat"
        erc-user-full-name "Wanderson Ferreira"
        erc-prompt-for-nickserv-password nil
        erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#clojure" "#systemcrafters"))
        erc-autojoin-timing :ident
        erc-autojoin-delay 40
        erc-join-buffer 'bury
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
        erc-track-exclude-server-buffer t
        erc-keywords '("programming" "functional" "design"))

  ;; show only when my nickname is mentioned in any channel
  (setq erc-current-nick-highlight-type 'nick
        erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE")
        erc-track-use-faces t
        erc-track-faces-priority-list '(erc-current-nick-face
                                        erc-keyword-face
                                        erc-direct-msg-face)
        erc-track-priority-faces-only 'all)

  ;; prevent the new created buffer to be brought visible
  (setq erc-auto-query 'bury)
  :config

  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules)
  (add-hook 'erc-text-matched-hook 'erc-sound-if-not-server)

  (require 'erc-compat)
  (require 'erc-nicklist)
  (setq erc-nicklist-icons-directory "~/.emacs.d/images/"))

(provide 'setup-chat)
;;; setup-chat.el ends here
