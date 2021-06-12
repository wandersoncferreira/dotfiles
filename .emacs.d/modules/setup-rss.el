;;; setup-rss --- RSS Feeds
;;
;;; Commentary:
;;
;;; Code:

;; * Functions

(defun bk/elfeed-disable-mode-setup ()
  "Some packages that I want to disable when reading rss feeds."
  (interactive)
  (setq-local right-margin-width 15
              left-margin-width 15)
  (abbrev-mode -1)
  (yas-minor-mode -1)
  (dired-async-mode -1)
  (global-auto-revert-mode -1))

(defun ambrevar/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defun elfeed-show-play-enclosure (enclosure-index)
  "Play podcast with mpv from ENCLOSURE-INDEX."
  (interactive (list (elfeed--enclosure-maybe-prompt-index elfeed-show-entry)))
  (let ((url (car
              (elt
               (elfeed-entry-enclosures elfeed-show-entry)
               (- enclosure-index 1)))))
    (async-shell-command (format "mpv '%s'" url) "*podcast*")))

;; * External Dependencies

(use-package elfeed
  :ensure t
  :commands (elfeed elfeed-update)
  :init
  (setq-default elfeed-search-filter "@3-week-ago +unread")
  :config
  (add-hook 'elfeed-show-mode-hook 'bk/elfeed-disable-mode-setup)
  (add-hook 'elfeed-search-update-hook 'bk/elfeed-disable-mode-setup))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")
        rmh-elfeed-org-tree-id "elfeed")
  :config
  (elfeed-org))

;; * Keybindings

(eval-after-load "elfeed"
  '(progn
     (define-key elfeed-search-mode-map "v" #'ambrevar/elfeed-play-with-mpv)
     (define-key elfeed-show-mode-map "v" #'ambrevar/elfeed-play-with-mpv)))

(provide 'setup-rss)
;;; setup-rss.el ends here
