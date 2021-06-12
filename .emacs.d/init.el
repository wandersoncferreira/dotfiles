;;; init.el --- Wand's config  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <wand@hey.com>
;; URL: https://github.com/wandersoncferreira/dotfiles
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Here be dragons!
;;
;;; Code:

;;; Security

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (ignore param)
     (list start end)))

;; elisp configuration folder
(setq modules-dir (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path modules-dir)

;; configurations specific to places I work at
(setq work-dir (expand-file-name "work" user-emacs-directory))
(add-to-list 'load-path work-dir)

(set-register ?t '(file . "/home/wanderson/agenda/todo.org"))
(set-register ?m '(file . "~/.emacs.d/manual.org"))
(set-register ?l '(file . "/home/wanderson/ledger"))
(set-register ?e '(file . "/home/wanderson/.emacs.d/init.el"))

;; * Packages

(use-package smex :ensure t)

;; * Imports

(require 'setup-defaults)
(require 'setup-keybindings)
(require 'setup-appearance)
(require 'setup-presentation)
(require 'setup-completion)
(require 'setup-dired)
(require 'setup-git)
(require 'setup-org)
(require 'setup-programming)
(require 'setup-search)
(require 'setup-eshell)
(require 'setup-auth)
(require 'setup-projects)
(require 'setup-editor)
(require 'setup-spell)

;; workplace
(require 'setup-work)
(require 'reifyhealth)
(require 'captalys)
(require 'appsauce)

;; third-party apps
(require 'setup-finance)
(require 'setup-chat)
(require 'setup-zettelkasten)

;; languages
(require 'setup-clojure)
(require 'setup-java)
(require 'setup-python)
(require 'setup-typescript)
(require 'setup-sql)
(require 'setup-racket)
(require 'setup-lisp)

(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-lisp-eval-depth (* 15 max-lisp-eval-depth))

;; transparently open compressed files
(auto-compression-mode t)

(load custom-file)

(windmove-default-keybindings)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(delete-selection-mode +1)

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

(use-package vlf :ensure t)

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "M-s-<up>") 'buf-move-up)
  (global-set-key (kbd "M-s-<down>") 'buf-move-down)
  (global-set-key (kbd "M-s-<left>") 'buf-move-left)
  (global-set-key (kbd "M-s-<right>") 'buf-move-right))

;;; Bookmarks

(use-package bm
  :ensure t
  :custom-face
  (bm-persistent-face ((t (:background "khaki2"))))
  :init
  (setq bm-restore-repository-on-load t
        bm-repository-file "~/.emacs.d/bm-repository"
        bm-buffer-persistence t
        bm-cycle-all-buffers t)
  :config
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook (lambda ()
                               (bm-buffer-save-all)
                               (bm-repository-save)))
  ;; restoring bookmarks
  (add-hook 'find-file-hook #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  (global-unset-key (kbd "<f2>"))
  (global-set-key (kbd "<C-f2>") 'bm-next)
  (global-set-key (kbd "<f2>") 'bm-toggle)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;;; Buffers

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t
        winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"
          "*kaocha-error*"))
  :config
  (winner-mode +1)
  (global-set-key (kbd "C-x 4 u") 'winner-undo)
  (global-set-key (kbd "C-x 4 U") 'winner-redo))

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '(("*kaocha-error*" :ignore t)))
  (shackle-mode +1))

(use-package recentf
  :config
  (recentf-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator " * "
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t
        uniquify-ignore-buffers-re "^\\*"))

;; automatically group all of your Emacs buffers into workspaces by defining a series of
;; grouping rules. I find this a lot better than perspective-mode which I have to manually
;; add buffers to each workspace.

(use-package bufler
  :ensure t
  :bind
  (("C-c b" . bufler-switch-buffer)
   ("C-c s f" . bufler-workspace-frame-set))
  :config
  (bufler-mode +1)
  (setf bufler-groups
        (bufler-defgroups
          (group
           (auto-projectile))
          (auto-directory))))

(use-package crux
  :ensure t
  :config
  (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c d l") #'crux-duplicate-current-line-or-region)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

;; automatically indenting yanked text if in programming modes
(require 'dash)

(defvar yank-indent-modes '(prog-mode
                            sgml-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation from BEG and END, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defun yank-unindented ()
  "Unindent after yank in programming modes."
  (interactive)
  (yank 1))


;;; Window

(defun bk/toggle-window-split ()
  "Toggle window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") 'bk/toggle-window-split)

;;; Emacs Movement

(defun bk/jump-to-register ()
  "Switch between current position and pos stored."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(defun bk/point-to-register ()
  "Store cursor position in a register."
  (interactive)
  (point-to-register 8)
  (message "Point set"))

(global-set-key (kbd "C-c m p") 'bk/point-to-register)
(global-set-key (kbd "C-c j p") 'bk/jump-to-register)

;;; use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;;; Markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))

(use-package subword
  :diminish subword-mode
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))


;;; Self-Discovering Tools

(use-package eldoc
  :diminish eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil)
  :config
  (global-eldoc-mode +1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; Custom Functions

(defun bk/packages-count ()
  "How many packages do I have installed."
  (interactive)
  (message "Packages installed: %s" (length package-alist)))

(defun bk/align-whitespace (start end)
  "Align columns by whitespace from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun bk/align-ampersand (start end)
  "Align columns by ampersand from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun bk/align-quote-space (start end)
  "Align columns by quote and space from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))

(defun bk/align-equals (start end)
  "Align columns by equals sign from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))

(defun bk/align-comma (start end)
  "Align columns by comma from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))

(defun bk/align-dot (start end)
  "Align columns by dot from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))

(defun bk/align-colon (start end)
  "Align columns by equals sign from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))

(defun bk/shame-on-you ()
  "Shame."
  (interactive)
  (message "Stop this bad habbit!"))

(defun bk/days-since ()
  "Number of days since DATE to today."
  (interactive)
  (let* ((date (read-from-minibuffer "Inicial date: "))
         (date-zoned (format "%s +0300" date)))
    (message (format "%s days ago" (days-between
                                    (current-time-string)
                                    date-zoned)))))

(defun bk/kill-all-comments ()
  "Function to kill all comments in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

(defun bk/point-to-register ()
  "Store cursor position in a register."
  (interactive)
  (point-to-register 8)
  (message "Point set"))

(defun bk/jump-to-register ()
  "Switch between current position and pos stored."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(global-set-key (kbd "C-c r p") #'bk/point-to-register)
(global-set-key (kbd "C-c j p") #'bk/jump-to-register)

(defun bk/clear-registers ()
  "Remove all saved registers."
  (interactive)
  (setq register-alist nil))

(defun bk/kill-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'bk/kill-buffer)

(defun bk/indent-buffer ()
  "Fix indentation of buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun bk/untabify-buffer ()
  "Remove tabs from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun bk/touch-buffer-file ()
  "Touch buffer."
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

(use-package zygospore
  :ensure t
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

(defun bk/vsplit-last-buffer ()
  "Split the window vertically and display the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'bk/vsplit-last-buffer)

(defun bk/hsplit-last-buffer ()
  "Split the window horizontally and display the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 3") 'bk/hsplit-last-buffer)

(defun bk/insert-date-today ()
  "Insert today date as YYYY-MM-DD."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (kill-new today)
    (insert today)))

(defun bk/unfill-paragraph ()
  "Takes a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun bk/ip ()
  "Find my current public IP address."
  (interactive)
  (let* ((endpoint "https://api.ipify.org")
         (myip (with-current-buffer (url-retrieve-synchronously endpoint)
                 (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))
    (kill-new myip)
    (message "IP: %s" myip)))


(use-package popup :ensure t)

;;; Emacs Server

(require 'server)
(unless (server-running-p)
  (server-start))

(defun bk/server-shutdown ()
  "Save buffers, quit, and shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;; XML

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4)
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode)))


;;; nixOS

(use-package nix-mode
  :ensure t)

;;; PDF

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-continuous-scroll-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
  (require 'pdf-occur))

;;; PlantUML

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :config
  (setq org-plantuml-jar-path "~/.emacs.d/bin/plantuml.jar"
        plantuml-default-exec-mode 'jar)
  (require 'ob-plantuml))

(use-package flycheck-plantuml
  :ensure t
  :after flycheck
  :config
  (flycheck-plantuml-setup))


;;; Misc. Custom Functions

(use-package windresize
  :ensure t)

;;; RSS Feed

(defun bk/elfeed-disable-mode-setup ()
  "Some packages that I want to disable when reading rss feeds."
  (interactive)
  (setq-local right-margin-width 15
              left-margin-width 15)
  (abbrev-mode -1)
  (yas-minor-mode -1)
  (dired-async-mode -1)
  (global-auto-revert-mode -1))

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

(eval-after-load "elfeed"
  '(progn
     (define-key elfeed-search-mode-map "v" #'ambrevar/elfeed-play-with-mpv)
     (define-key elfeed-show-mode-map "v" #'ambrevar/elfeed-play-with-mpv)))

;;; Media

(use-package helm-spotify-plus
  :ensure t)

;;; API

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))


(defun bk/restclient ()
  "Open a restclient buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))

;;; Calendar and Diary

(defface prot-diary-calendar-administrative-mark
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :background "#221000" :foreground "#eecc00")
    (t :foreground "yellow"))
  "Face to mark administrative tasks in the calendar.")

(defface prot-diary-calendar-mundane-mark
  '((((class color) (min-colors 88) (background light))
     :background "#f0f0f0" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :background "#191a1b" :foreground "#a8a8a8")
    (t :inherit shadow))
  "Face to mark mundane tasks in the calendar.")


(defface prot-diary-calendar-event-mark
  '((((class color) (min-colors 88) (background light))
     :background "#aceaac" :foreground "#004c00")
    (((class color) (min-colors 88) (background dark))
     :background "#00422a" :foreground "#9ff0cf")
    (t :foreground "green"))
  "Face to mark events in the calendar.")

(use-package calendar
  :config
  (setq calendar-mark-diary-entries-flag t
        calendar-mark-holidays-flag nil
        calendar-mode-line-format nil
        calendar-time-display-form '(24-hours ":" minutes (when time-zone (format "(%s)" time-zone)))
        calendar-week-start-day 1 ;; monday
        calendar-date-style 'iso
        calendar-date-display-form calendar-iso-date-display-form
        calenadr-time-zone-style 'numeric))

(use-package diary-lib
  :config
  (setq diary-date-forms diary-iso-date-forms
        diary-comment-start ";;"
        diary-comment-end ""
        diary-nonmarking-symbol "!"
        aadiary-show-holidays-flag t
        diary-display-function #'diary-fancy-display
        diary-header-line-format nil
        diary-number-of-entries 2
        diary-mail-days 2
        diary-abbreviated-year-flag nil))

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-mode-hook #'goto-address-mode)


(require 'cal-dst)
(setq calendar-standard-time-zone-name "-0300")
(setq calendar-daylight-time-zone-name "-0300")

;; notification of appointments from your diary file
(use-package appt
  :init
  (setq appt-checking-p t
        appt-display-diary nil
        appt-disp-window-function #'appt-disp-window
        appt-display-mode-line t
        appt-display-interval 5
        appt-warning-time-regexp "appt \\([0-9]+\\)"
        appt-message-warning-time 10)
  :config
  (run-at-time 10 nil #'appt-activate 1)
  (add-hook 'diary-hook 'appt-make-list))


;;; Manage external services

(use-package prodigy
  :ensure t
  :bind
  (("C-c s l" . prodigy))
  :config
  (prodigy-define-service
    :name "tempo"
    :command "make"
    :cwd "~/repos/experiments/tempo/"
    :stop-signal 'sigkill
    :tags '(eas-tempo)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "esource-repl"
    :command "make"
    :args '("repl")
    :cwd "~/repos/reifyhealth/esource-service/"
    :stop-signal 'sigkill
    :tags '(esource-repl)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "esource"
    :command "make"
    :args '("run")
    :cwd "~/repos/reifyhealth/esource-service/"
    :stop-signal 'sigkill
    :tags '(esource-run)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "study-sheet"
    :command "yarn"
    :args '("start")
    :cwd "~/repos/reifyhealth/study-sheet/"
    :stop-signal 'sigkill
    :tags '(stdocs)
    :kill-process-buffer-on-stop t)

  (prodigy-define-tag
    :name 'eas-tempo
    :ready-message ".*Build completed.*")

  (prodigy-define-tag
    :name 'esource-repl
    :ready-message "nREPL started!")

  (prodigy-define-tag
    :name 'stdocs
    :ready-message "Starting the development server...")

  (prodigy-define-tag
    :name 'esource-run
    :ready-message "WARNING: .*"))

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;;; Writing

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-extra-line-spacing t
        writeroom-mode-line t
        writeroom-restore-window-config t
        writeroom-fullscreen-effect 'maximized))


;;; EPUB

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))


;;; End of file

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved cl-functions)
;; End:

;;; init.el ends here
