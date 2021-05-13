;;; init.el --- Wand's config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Here be dragons!

;;; Code:

;;; Security
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (ignore param)
     (list start end)))


;;; Emacs basic

(set-register ?t '(file . "/home/wanderson/agenda/todo.org"))
(set-register ?l '(file . "/home/wanderson/ledger"))
(set-register ?e '(file . "/home/wanderson/.emacs.d/init.el"))

(defun bk/eval-buffer ()
  "Provide some feedback after evaluating the buffer."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(use-package emacs
  :init
  (setq tab-always-indent 'complete
        ring-bell-function 'ignore
        visible-bell nil
        create-lockfiles nil
        custom-safe-themes t
        indent-tabs-mode nil
        delete-by-moving-to-trash t ;move files to trash when deleting
        echo-keystrokes 0.1         ;show keystrokes in progress
        tab-width 4
        make-backup-files nil
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 4 1024 1024)
        custom-file (expand-file-name "custom.el" user-emacs-directory))
  :bind (("C-x p" . pop-to-mark-command)
         ("C-x C-b" . ibuffer)
         ("C-x e" . eshell)
         :map emacs-lisp-mode-map
         ("C-c C-k" . bk/eval-buffer)))

(add-hook 'comint-mode-hook 'turn-on-visual-line-mode)

;; scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-lisp-eval-depth (* 15 max-lisp-eval-depth))

;; handling long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; disable dialog boxes
(setq use-dialog-box nil)

;; disable file dialog
(setq use-file-dialog nil)

;; no ugly button for checkboxes
(setq widget-image-enable nil)

;; enable line number modes
(dolist (mode '(prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(global-prettify-symbols-mode +1)

;;; make cursor the width of the character it is under
(setq x-stretch-cursor t)

;; more complex clipboard management
(setq save-interprogram-paste-before-kill t)

;;; real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; don't warn for following symlinked files
(setq vc-follow-symlinks t)

;; ellispis are nice
(setq truncate-string-ellipsis "…")

;; don't warn when advice is added for functions
(setq ad-redefinition-action 'accept)

;; user configs
(setq user-mail-address "wand@hey.com"
      user-full-name "Wanderson Ferreira")

;; fewer slots for mark rings
(setq mark-ring-max 4)

(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Sao_Paulo" "Sao Paulo")
        ("Europe/Paris" "Paris")
        ("America/Boston" "Boston")))

;; truncate lines
(setq-default truncate-lines t)

;; transparently open compressed files
(auto-compression-mode t)

;; show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; lines should be 80 characters wide, not 72
(setq fill-column 80)
(setq fci-rule-column 80)

;; save minibuffer history
(savehist-mode +1)
(setq history-length 1000
      history-delete-duplicates nil
      savehist-additional-variables '(search-ring regexp-search-ring))

;; show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; sentences do not need double spaces to end
(set-default 'sentence-end-double-space nil)

(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package paren
  :init
  (setq show-paren-style 'parenthesis)
  :config
  (show-paren-mode +1))

(load custom-file)

(windmove-default-keybindings)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

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
  :bind
  (("M-s-<up>" . buf-move-up)
   ("M-s-<down>" . buf-move-down)
   ("M-s-<left>" . buf-move-left)
   ("M-s-<right>" . buf-move-right)))

;;; Abbreviations

(defun bk/add-region-local-abbrev (start end)
  "Go from START to END and add the selected text to a local abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-mode-abbrev num-words)
        (deactivate-mark))
    (message "No selected region!")))

(defun bk/add-region-global-abbrev (start end)
  "Go from START to END and add the selected text to global abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-abbrev global-abbrev-table "Global" num-words)
        (deactivate-mark))
    (message "No selected region!")))

(define-abbrev-table 'global-abbrev-table
  '(
    ("reuslt" "result" nil 0)
    ("requie" "require" nil 0)
    ("requier" "require" nil 0)
    ))

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(add-hook 'after-init-hook (lambda ()
                             (abbrev-mode +1)
                             (diminish 'abbrev-mode)))

;;; Saving data between sessions

(use-package session
  :ensure t
  :init
  (setq session-use-package t)
  :config
  (add-hook 'after-init-hook 'session-initialize))


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

;;; Authentication
(use-package pinentry
  :ensure t
  :init
  (setq epg-gpg-program "gpg"
        epa-file-cache-passphrase-for-symmetric-encryption t
        epa-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(use-package epa
  :ensure nil
  :init
  (setq auth-source-debug t
        auth-sources '((:source "~/.secrets/authinfo.gpg")))
  :config
  (pinentry-start))

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

;;; Eshell

(defun eshell-clear-buffer ()
  "Clear the terminal buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")))

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
          "*esh command on file*"))
  :config
  (winner-mode +1)
  (global-set-key (kbd "C-x 4 u") 'winner-undo)
  (global-set-key (kbd "C-x 4 U") 'winner-redo))

(use-package recentf
  :config
  (recentf-mode +1))

(use-package replace
  :bind
  ("C-c o" . #'bk/occur-dwim))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator " * "
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t
        uniquify-ignore-buffers-re "^\\*"))

(defun bk/ansi-colorize-buffer ()
  "Eliminate weird escape sequences during compilation of projects."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'bk/ansi-colorize-buffer))

;;; Dired

(defun bk/dired-directories-first ()
  "Sorted dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defun bk/dired-xdg-open ()
  "Open the file at point with xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Openning %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun bk/dired-back-to-start-of-files ()
  "Move to start of line."
  (interactive)
  (backward-char (- (current-column) 2)))

(defun bk/dired-back-to-top ()
  "Go back to correct position at the top."
  (interactive)
  (goto-char (point-min))
  (forward-line 2)
  (bk/dired-back-to-start-of-files))

(defun bk/dired-back-to-bottom ()
  "Go back to correct position at the bottom."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (bk/dired-back-to-start-of-files))

(use-package dired
  :bind
  (:map dired-mode-map
        ("O" . bk/dired-xdg-open)
        ("M-p" . bk/dired-back-to-top)
        ("M-n" .  bk/dired-back-to-bottom)
        ("C-a" .  bk/dired-back-to-start-of-files)
        ("C-x C-k" . dired-do-delete)
        ("k" . dired-do-delete))
  :init
  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  :config
  (require 'dired-x)
  (setq dired-dwim-target t)
  (advice-add 'dired-readin :after #'bk/dired-directories-first))

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

;;; Ido Completion

(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-everywhere t
        ido-show-dot-for-dired t
        ido-create-new-buffer 'always
        confirm-nonexistent-file-or-buffer nil
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion t
        ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" "
  [No match]" " [Matched]" " [Not readable]" " [Too big]" "
  [Confirm]"))
        confirm-nonexistent-file-or-buffer nil
        )
  :bind (:map ido-file-completion-map
              ("C-n" . ido-next-match)
              ("C-p" . ido-prev-match)
              ("C-w" . ido-delete-backward-updir)
              ("C-x C-w" . ido-copy-current-file-name)
              :map ido-common-completion-map
              ("SPC" . self-insert-command)
              ("M-SPC" . just-one-space)
              :map ido-file-dir-completion-map
              ("C-w" . ido-delete-backward-updir)
              ("C-x C-w" . ido-copy-current-file-name))
  :config
  (ido-mode +1)
  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules")
  (add-to-list 'ido-ignore-directories ".cpcache")
  (add-to-list 'ido-ignore-directories "eln-cache")
  (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command))

(use-package ido-at-point
  :ensure t
  :config
  (ido-at-point-mode +1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-x C-m" . smex)
         ("C-c C-m" . smex)))

(use-package ido-completing-read+
  :ensure t
  :after ido
  :config
  (ido-ubiquitous-mode +1))

;; hippie expand

(require 'patch-hippie-expand)

(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-c l") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;;; Appearance

(defun bk/search-font (font-name)
  "Search for a FONT-NAME."
  (-filter (lambda (f) (string-match font-name f)) (font-family-list)))

(defun bk/set-ibm-font (size)
  "Set default font at SIZE."
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height size))

(use-package simple
  :init
  (setq inhibit-startup-screen nil)
  :config
  (set-face-attribute 'lazy-highlight nil :background "khaki1")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1")

  (add-to-list 'default-frame-alist '(background-color . "honeydew"))
  (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))

  (bk/set-ibm-font 100)
  (set-background-color "honeydew"))

;; large fringes to get high-resolution flycheck marks
(fringe-mode '(16 . 16))

(defvar bk--toggle-transparency nil)

(defun bk/toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (if bk--toggle-transparency
      (progn
        (set-frame-parameter (selected-frame) 'alpha 100)
        (setq bk--toggle-transparency nil))
    (progn
      (set-frame-parameter (selected-frame) 'alpha 90)
      (setq bk--toggle-transparency t))))


;;; Projects

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ido
        projectile-dynamic-mode-line nil
        projectile-ignored-projects '("~/" "/tmp")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "TAGS" "ido.last" "recentf" "smex-items")
        )
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (require 'subr-x)
  (projectile-mode +1))

(use-package perspective
  :ensure t
  :init
  (setq persp-initial-frame-name "Main")
  :config
  (unless (equal persp-mode t)
    (persp-mode)))

;; find file in project, with specific patterns

(defun ffip--create-exclude-find-options (names)
  "Exclude NAMES from find candidates."
  (mapconcat (lambda (name) (concat "-not -regex \".*" name ".*\"")) names " "))

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-find-options
        (ffip--create-exclude-find-options
         '("/node_modules/"
           "/target/"
           "/out/"
           "/.shadow-cljs/"
           "/.cpcache/"))))

(defun ffip-create-pattern-file-finder (&rest patterns)
  "Create new functions that look for a specific PATTERNS."
  (let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o jn") (ffip-create-pattern-file-finder "*.json"))
(global-set-key (kbd "C-x C-o ht") (ffip-create-pattern-file-finder "*.html"))
(global-set-key (kbd "C-x C-o ed") (ffip-create-pattern-file-finder "*.edn"))
(global-set-key (kbd "C-x C-o ym") (ffip-create-pattern-file-finder "*.yml"))


;;; Emacs Editor

;; auto-detecting indentation style
(use-package dtrt-indent
  :ensure t
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-mode +1))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w"))

;; move around through points at which you made edits in a buffer here
(use-package goto-chg
  :ensure t
  :config
  (global-set-key (kbd "C-c b ,") 'goto-last-change)
  (global-set-key (kbd "C-c b .") 'goto-last-change-reverse))

(use-package expand-region
  :ensure t
  :config
  (setq expand-region-fast-keys-enabled nil
        er--show-expansion-message t)
  :bind ("C-=" . er/expand-region))

(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package fix-word
  :ensure t
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("<mouse-3>" . mc/toggle-cursor-on-click)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode +1))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(defun bk/kill-inner-word ()
  "Kill the entire word your cursor is in.  Equivalent to ciw in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "C-c k w") 'bk/kill-inner-word)

(defun duplicate-region (num &optional start end)
  "Duplicate the region bounded by START and END NUM times."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (_ num)
      (insert region))))

(defun paredit-duplicate-current-line ()
  "Duplicate current line if paredit-mode is enabled."
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1)))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))

(defun bk/duplicate-current-line-or-region (arg)
  "Duplicate the current line or region ARG times."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

(global-set-key (kbd "C-c d l") #'bk/duplicate-current-line-or-region)


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

(use-package imenu
  :config
  (setq imenu-auto-rescan 1
        imenu-auto-rescan-maxout 600000
        imenu-max-item-length 600
        imenu-use-markers t
        imenu-max-items 200)
  :bind
  ("C-c i" . imenu))

(defun bk/occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(use-package jump-char
  :ensure t
  :bind (("M-n" . jump-char-forward)
         ("M-p" . jump-char-backward)))

;;; Git

(use-package magit
  :ensure t
  :init
  (setq magit-log-show-gpg-status t
        magit-completing-read-function 'magit-ido-completing-read)
  :bind (("C-c g s" . magit-status)
         ("C-c g b" . magit-blame))
  :config
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes unstage-all-changes))

  ;; Real dates, please
  (set-default 'magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode +1))

(use-package forge
  :ensure t
  :config
  (setq forge-topic-list-limit '(10 . -1))
  :bind
  ("C-c f r" . forge-list-requested-reviews)
  ("C-c f p" . forge-list-authored-pullreqs)
  ("C-c f c" . forge-create-pullreq))

(use-package git-timemachine :ensure t)

(use-package gitconfig-mode
  :ensure t
  :config
  (require 'gitconfig-mode))

(use-package gitignore-mode
  :ensure t
  :config
  (require 'gitignore-mode))

(use-package gitignore-templates
  :ensure t)

(use-package browse-at-remote
  :ensure t)

(use-package gist
  :ensure t
  :commands (gist-region-or-buffer))

;;; General Programming

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode +1))

(use-package toggle-test
  :ensure t
  :init
  (setq tgt-open-in-new-window nil)
  :config
  (put 'tgt-projects 'safe-local-variable #'lisp)
  (global-set-key (kbd "s-t") 'tgt-toggle))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-checker-error-threshold 4000))

(defun yas/goto-end-of-active-field ()
  "End of the field."
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  "Start of the field."
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-verbosity 1
        yas-wrap-around-region t)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)

  ;; jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  :bind
  (("C-x y" . yas-expand)
   ("C-c y" . yas-expand)
   ("C-c t" . yas-describe-tables)))

;;; use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(use-package quickrun :ensure t)

;;; Emacs Lisp

(use-package outline
  :diminish outline-minor-mode
  :init
  (setq outline-blank-line t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (global-set-key (kbd "C-c h") 'outline-cycle))

(use-package bicycle
  :ensure t
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle-global)
              ("<backtab>" . bicycle-cycle)))

;;; Lisps

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (with-eval-after-load "eldoc"
    (eldoc-add-command #'paredit-backward-delete #'paredit-close-round)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

;;; Markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))

;;; Clojure

(use-package clojure-snippets :ensure t)

(defun bk/nrepl-warn-when-not-connected ()
  "Function to warn me to start the REPL."
  (interactive)
  (message "Oops! You're not connected to an nREPL server.
Please run M-x cider or M-x cider-jack-in to connect"))

(defun bk/cider-quit-all ()
  "Iterate over all CIDER buffers and close them all."
  (interactive)
  (let ((repl-buffers (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (eq major-mode 'cider-repl-mode)))
                                  (buffer-list))))
    (dolist (buf repl-buffers)
      (cider--close-connection buf)
      (kill-buffer buf))
    (message "All CIDER buffers were closed.")))

(use-package symbol-focus
  :load-path "~/.emacs.d/lisps"
  :bind
  ("C-c f f" . sf/focus-at-point)
  :config
  (symbol-focus-mode +1))

(use-package clojure-mode
  :ensure t
  :init
  (setq clojure-toplevel-inside-comment-form t)
  :config
  (use-package subword :diminish subword-mode)
  (add-hook 'clojure-mode-hook #'subword-mode) ;; deal with java class and method names
  )

(use-package kaocha-runner
  :ensure t
  :init
  (setq kaocha-runner-repl-invocation-template
        "(do (require 'kaocha.repl) %s)")
  :config
  (require 'patch-kaocha-runner)
  :bind
  (:map clojure-mode-map
        ("C-c k t" . kaocha-runner-run-test-at-point)
        ("C-c k r" . kaocha-runner-run-tests)
        ("C-c k a" . kaocha-runner-run-all-tests)
        ("C-c k w" . kaocha-runner-show-warnings)
        ("C-c k h" . kaocha-runner-hide-windows)))

(use-package cider
  :ensure t
  :config
  (setq cider-save-file-on-load t
        cider-annotate-completion-candidates nil ;; disable completion annotations
        cider-prompt-for-symbol nil
        cider-eldoc-display-context-dependent-info t  ;; try to add expected function arguments
        cider-font-lock-dynamically '(macro core function var)
        cider-prefer-local-resources t
        cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources"
                              "~/Downloads/jvm11/source")
        cider-print-fn 'pprint
        cider-print-options
        '(("length" 80)
          ("level" 20)
          ("right-margin" 80))))

(defun bk/reload-cider-completion ()
  "Function to reload cider completion.
Better naming to improve the chances to find it."
  (interactive)
  (cider-completion-flush-caches))

(use-package clj-refactor
  :ensure t
  :init
  (setq cljr-warn-on-eval t
        cljr-eagerly-build-asts-on-startup t
        cljr-favor-prefix-notation nil

        ;; execute `cljr-clean-ns' for all .clj files.
        cljr-project-clean-prompt nil

        cljr-favor-private-functions t

        ;; whitelist, do not clean this libspec
        cljr-libspec-whitelist
        '("^moment")

        cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("set" . "clojure.set")
          ("str" . "clojure.string")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("time" . "clj-time.core")
          ("log" . "clojure.tools.logging")
          ("json" . "cheshire.core"))
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is use-fixtures]]")
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'cider-mode-hook 'clj-refactor-mode)

  ;; refactor functionalities available in clojure mode will be added here too
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "aa") 'clojure-add-arity)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "c!") 'clojure-cycle-not)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "cw") 'clojure-cycle-when)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "ra") 'clojure-rename-ns-alias)
  )

;; Experimental configuration to hotload refactor
;; using Pomegranate from Cemerick
;; From https://www.eigenbahn.com/2020/05/06/fast-clojure-deps-auto-reload
;; and integrated into clj-refactor.

(eval-after-load 'cider
  '(progn
     (defun bk/send-to-repl (sexp eval ns)
       (ignore eval)
       (cider-switch-to-repl-buffer ns)
       (goto-char cider-repl-input-start-mark)
       (delete-region (point) (point-max))
       (save-excursion
         (insert sexp)
         (when (equal (char-before) ?\n)
           (delete-char -1))
         (cider-repl--send-input t))
       (delete-region (point) (point-max)))

     (defun bk/pomegranate-dep (dep)
       (concat
        (format
         "%s"
         `(use '[cemerick.pomegranate :only (add-dependencies)]))
        (s-replace-all
         `(("\\." . ".")
           ("mydep" . ,dep))
         (format
          "%S"
          `(add-dependencies :coordinates '[mydep]
                             :repositories (merge cemerick.pomegranate.aether/maven-central
                                                  {"clojars" "https://clojars.org/repo"}))))))

     (setq cljr-hotload-dependencies t)

     (defun cljr-hotload-dependency (artifact version &optional dep ns)
       (ignore dep)
       (bk/send-to-repl
        (bk/pomegranate-dep (format "[%s \"%s\"]" artifact version))
        t ns))

     (defun cljr--add-project-dependency (artifact version)
       (let* ((project-file (cljr--project-file))
              (deps (cljr--project-with-deps-p project-file)))
         (cljr--update-file project-file
           (goto-char (point-min))
           (if deps
               (cljr--insert-into-clj-dependencies artifact version)
             (cljr--insert-into-leiningen-dependencies artifact version))
           (cljr--post-command-message "Added %s version %s as a project dependency" artifact version))
         (when cljr-hotload-dependencies
           (if deps
               (back-to-indentation)
             (paredit-backward-down))
           (cljr-hotload-dependency artifact version))))))

(eval-after-load 'projectile
  '(progn
     (pushnew! projectile-globally-ignored-directories ".cpcache")))


;;; Typescript

(defun bk/setup-tide-mode ()
  "Setup js development."
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (electric-pair-mode +1)
  (tide-hl-identifier-mode +1)
  (prettier-js-mode +1)
  (add-node-modules-path))

(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx\\'" . typescript-mode)
  :hook (typescript-mode . bk/setup-tide-mode))

(use-package prettier-js :ensure t)
(use-package add-node-modules-path :ensure t)
(setq-default js-indent-level 2)

(use-package tide
  :ensure t
  :diminish tide-mode
  :init
  (setq tide-completion-detailed t
        tide-always-show-documentation t
        tide-server-max-response-length 524288)
  :hook ((js-mode . bk/setup-tide-mode)
         (js2-mode . bk/setup-tide-mode)
         (typescript-mode . bk/setup-tide-mode)))

(eval-after-load 'projectile
  '(progn
     (pushnew! projectile-project-root-files "package.json")
     (pushnew! projectile-globally-ignored-files "node_modules" "bkp" ".log")
     (pushnew! projectile-globally-ignored-directories "node_modules" "bkp" ".log")))


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

(defun bk/clear-registers ()
  "Remove all saved registers."
  (interactive)
  (setq register-alist nil))

(defun what-face (pos)
  "Find what face at POS."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun bk/kill-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'bk/kill-buffer)

(defun bk/kill-buffer-and-file (buffer-name)
  "Remove file connected to current buffer and kill the BUFFER-NAME."
  (interactive "bKill buffer and its file: ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" buffer-name)
      (delete-file filename)
      (kill-buffer buffer))))

(defun bk/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun bk/beginning-of-line ()
  "Go back at the first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'bk/beginning-of-line)

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

(defun bk/copy-ledger-entry ()
  "Copy last ledger entry."
  (interactive)
  (save-excursion
    (backward-sentence)
    (let ((beg (point)))
      (forward-sentence)
      (kill-ring-save beg (point))))
  (yank))

(defun bk/clean-ledger ()
  "Bring back timeline structure to the whole file."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

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

(defun bk/create-worktree ()
  "Help development on multiple branches."
  (interactive)
  (let* ((root-proj (projectile-project-root))
         (proj-name (car (cdr (nreverse (split-string root-proj "/")))))
         (dest-dir (file-name-directory (directory-file-name root-proj)))
         (branch (ido-completing-read "Choose the branch: " (magit-list-local-branch-names)))
         (worktree-path (concat dest-dir proj-name "-wt-" branch)))
    (magit-worktree-checkout worktree-path branch)
    (projectile-find-file)))

(defun bk/delete-worktree ()
  "Delete worktree and all its open buffers."
  (interactive)
  (let ((worktree (ido-completing-read "Choose worktree: " (magit-list-worktrees))))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (let ((worktree-name (file-name-base worktree)))
                (when (string-equal (projectile-project-name) worktree-name)
                  (kill-buffer buffer)))))
          (buffer-list))
    (magit-worktree-delete worktree)))

(defun bk/spell-buffer-pt-BR ()
  "Spell check in portuguese."
  (interactive)
  (ispell-change-dictionary "pt_BR")
  (flyspell-buffer))

(defun bk/spell-buffer-en ()
  "Spell check in English."
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))


;;; Spell Checking

(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar
        "~/.emacs.d/bin/languagetool-commandline.jar"))

(use-package google-this :ensure t)

(use-package popup :ensure t)

;;; Org mode

(use-package org
  :ensure nil
  :init
  (setq org-return-follows-link t
        org-confirm-babel-evaluate nil
        org-replace-disputed-keys t ;don't ruin S-arrow to switch windows please
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-agenda-files (list "~/agenda/todo.org"))
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  (require 'ob-clojure)
  (require 'org-tempo)

  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)
     (clojure . t)))

  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "~/agenda/todo.org" "Task")
           "* TODO %^{Title}\n %i" :clock-in t :clock-resume t))
        ))

(use-package org-download
  :ensure t
  :config
  (setq org-image-actual-width nil))

;;; Zettelkasten

(defvar bk-zettelkasten-dir "/home/wanderson/zettelkasten")

(use-package org-roam
  :ensure t
  :diminish org-roam-mode
  :init
  (setq org-roam-directory bk-zettelkasten-dir
        org-roam-completion-system 'ido)
  :bind (("C-c z f" . org-roam-find-file)
         ("C-c z i" . org-roam-insert)
         ("C-c z I" . org-roam-insert-immediate))
  :config
  (org-roam-mode +1)
  (add-hook 'org-roam-backlinks-mode-hook 'toggle-truncate-lines))

(use-package org-roam-server
  :ensure t
  :commands (org-roam-server-mode)
  :init
  (setq org-roam-server-port 17042)
  :config
  (require 'org-protocol)
  (require 'org-roam-protocol))


;;; Emacs Server

(require 'server)
(unless (server-running-p)
  (server-start))

(defun bk/server-shutdown ()
  "Save buffers, quit, and shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;; Finance management
(use-package ledger-mode
  :ensure t
  :mode ("ledger" . ledger-mode)
  :init
  :custom
  (ledger-reports
   '(("netcash" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -R -X R$ --current bal ^assets:bank ^assets:crypto liabilities:card")
     ("sports" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:sports")
     ("doctor" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:doctor")
     ("apartamento-mae" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ -S date --current -w reg ^liabilities:apartment:mother")
     ("apartamento-misce" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ -S date --current -w reg ^liabilities:apartment:misce")
     ("eas-profit" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --invert --current bal ^expenses:eval ^income:eval")
     ("food" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:food")
     ("donation" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:donation")
     ("apartamento-morumbi" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:house")
     ("creta" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:car:creta ^equity:car:creta")
     ("networth" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^assets:bank liabilities equity:apartment")
     ("spent-vs-earned" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger bal -X BRL --period=\"last 4 weeks\" ^Expenses ^Income --invert -S amount")
     ("budget" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^assets:bank:checking:budget liabilities:card")
     ("taxes" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -R -X R$ --current bal ^expenses:taxes")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :config
  (setq ledger-report-auto-width t))

(use-package hledger-mode
  :ensure t
  :bind (:map hledger-mode-map
              ("TAB" . completion-at-point)))

;;; Docker

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

;;; XML

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4)
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode)))

;;; SQL

(use-package sql-indent
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

;;; Search

(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t
        ag-reuse-window t))

(use-package rg
  :ensure t
  :commands (bk/search-git-root-or-dir)
  :config
  (rg-define-search bk/search-git-root-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc vc default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package wgrep :ensure t)

;;; nixOS

(use-package nix-mode :ensure t)


;;; Reify Health

(defun reifyhealth/cider-connect ()
  "Connect into eSource."
  (interactive)
  (cider-connect-clj (list :host "localhost" :port 12344)))

(defun reifyhealth ()
  "Open file notes from work."
  (interactive)
  (find-file "~/repos/reifyhealth/work.org"))


(defun work-new-day ()
  "Create entry into org file for bookkeeping."
  (interactive)
  (goto-char (point-max))
  (org-insert-heading-respect-content)
  (org-metaright)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
  (org-metaleft)
  (reindent-then-newline-and-indent)
  (reindent-then-newline-and-indent)
  (org-cycle)
  (insert "- "))

;;; PDF

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-occur))


;;; PlantUML
(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :init
  (setq org-plantuml-jar-path "~/.emacs.d/bin/plantuml.jar")
  :config
  (require 'ob-plantuml))


;;; Misc. Custom Functions

(use-package windresize
  :ensure t)

(use-package try :ensure t)

(use-package zprint-mode :ensure t)

(use-package uuidgen
  :preface
  (defun bk/uuid ()
    "Create uuid and add to clipboard."
    (interactive)
    (kill-new (uuidgen-4)))
  :ensure t)

;;; RSS Feed

(defun bk/elfeed-disable-mode-setup ()
  "Some packages that I want to disable when reading rss feeds."
  (interactive)
  (abbrev-mode -1)
  (yas-minor-mode -1)
  (dired-async-mode -1)
  (global-auto-revert-mode -1)
  (diff-hl-flydiff-mode -1))

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

(use-package calendar
  :config
  (setq calendar-mark-diary-entries-flag t
        calendar-mark-holidays-flag t
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

;;; Writing

(use-package flymd
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :init
  (setq writeroom-width 100
        writeroom-extra-line-spacing 1)
  :config
  (add-hook 'text-mode-hook 'auto-fill-mode))

;;; Manage external services

(use-package prodigy
  :ensure t
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

;;; Time tracking

(use-package wakatime-mode
  :ensure t
  :diminish wakatime-mode
  :config
  (global-wakatime-mode +1))

;; using projectile information to access Project in wakatime
(require 'projectile)
(defun wakatime-client-command (savep)
  "Return client command executable and arguments, set SAVEP to non-nil for write action."
  (format "%s%s--file \"%s\" --plugin \"%s/%s\" --project \"%s\" --time %.2f%s%s"
          (if (s-blank wakatime-python-bin) "" (format "\"%s\" " wakatime-python-bin))
          (if (s-blank wakatime-cli-path) "wakatime " (format "\"%s\" " wakatime-cli-path))
          (buffer-file-name (current-buffer))
          wakatime-user-agent
          wakatime-version
          (let ((pname (projectile-project-name)))
            (if (string= pname "-")
                "Other"
              pname))
          (float-time)
          (if savep " --write" "")
          (if (s-blank wakatime-api-key) "" (format " --key %s" wakatime-api-key))))

;;; End of file

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved cl-functions)
;; End:

;;; init.el ends here
