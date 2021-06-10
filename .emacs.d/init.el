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


;;; Emacs basic

(setq modules-dir (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path modules-dir)

(set-register ?t '(file . "/home/wanderson/agenda/todo.org"))
(set-register ?m '(file . "~/.emacs.d/manual.org"))
(set-register ?l '(file . "/home/wanderson/ledger"))
(set-register ?e '(file . "/home/wanderson/.emacs.d/init.el"))

(require 'setup-defaults)
(require 'setup-keybindings)
(require 'setup-appearance)
(require 'setup-completion)

(add-hook 'comint-mode-hook 'turn-on-visual-line-mode)

;; this will save the buffer for me...
(auto-save-visited-mode +1)
(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (save-some-buffers t))))

;; save buffer when you move between windows
(defadvice switch-to-buffer (before save-buffer-now activate)
  "Save buffer when move between windows."
  (when buffer-file-name (save-buffer)))

(defadvice other-window (before other-window-now activate)
  "Save buffer when move between windows."
  (when buffer-file-name (save-buffer)))

(defadvice other-frame (before other-frame-now activate)
  "Save buffer when move between windows."
  (when buffer-file-name (save-buffer)))

(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-lisp-eval-depth (* 15 max-lisp-eval-depth))

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

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package paren
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
  :config
  (global-set-key (kbd "M-s-<up>") 'buf-move-up)
  (global-set-key (kbd "M-s-<down>") 'buf-move-down)
  (global-set-key (kbd "M-s-<left>") 'buf-move-left)
  (global-set-key (kbd "M-s-<right>") 'buf-move-right))

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

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(add-hook 'after-init-hook
          (lambda ()
            (abbrev-mode +1)
            (diminish 'abbrev-mode)))

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
  (when (not (process-live-p pinentry--server-process))
    (pinentry-start)))

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


(use-package eshell-toggle
  :ensure t
  :bind ("C-x e" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package fish-completion
  :ensure t
  :hook (eshell-mode . fish-completion-mode))

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

(use-package replace
  :bind
  (("C-c o" . #'bk/occur-dwim)))

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

  ;; enable 'a'-keybinding in dired - which opens the file and closes dired buffer
  (put 'dired-find-alternate-file 'disabled nil)

  (advice-add 'dired-readin :after #'bk/dired-directories-first))

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-x C-m" . smex)
         ("C-c C-m" . smex)))

;; hippie expand

(require 'patch-hippie-expand)

(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-x l") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

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
  :config
  (require 'subr-x)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

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

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(use-package crux
  :ensure t
  :config
  (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c d l") #'crux-duplicate-current-line-or-region)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w"))

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
         ("<mouse-3>" . mc/add-cursor-on-click)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(trailing tabs tab-mark)
        whitespace-line-column 100)
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

;;; Emacs utils

(defun util/get-frame->selected-window ()
  "Return a list of pairs of (frame `selected-window')."
  (let* ((original-frame (window-frame))
         (result (->> (visible-frame-list)
                   (-map (lambda (f)
                           (select-frame f t)
                           (list f (selected-window)))))))
    (select-frame original-frame t)
    result))

(defun util/preserve-seelcted-window (f)
  "Run the given function F and then restore focus to the original window.
Useful when you want to invoke a function (like showing
documentation) but desire to keep your current window focused."
  (let* ((original-frame (selected-frame))
         (frames->windows (util/get-frame->selected-window))
         (result (funcall f)))
    (-each frames->windows (lambda (x)
                             (select-frame (first x) t)
                             (select-window (second x) t)))
    (select-frame-set-input-focus original-frame t)
    result))

;;; Git

(defun init-git-commit-mode ()
  "Move cursor to the beginning of the buffer when the git commit window is shown."
  (end-of-line))

(defun with-magit-output-buffer (f)
  "Display the result of function F in a new buffer."
  (let ((f f))
    (util/preserve-seelcted-window
     (lambda ()
       (funcall f)
       (display-buffer (magit-process-buffer t))))))

(defun git-pull ()
  "Run git pull in a dedicated buffer."
  (interactive)
  (with-magit-output-buffer (lambda () (call-interactively 'magit-pull-from-upstream))))

(use-package magit
  :ensure t
  :init
  (setq magit-log-show-gpg-status t
        magit-completing-read-function 'magit-ido-completing-read

        ;; when committing, don't have Magit show the diff of what's changed
        magit-commit-show-diff nil

        ;; don't refresh the status buffer unless it's currently focused. This should increase performace
        magit-refresh-status-buffer nil

        ;; have magit open buffers in the current window, rather than a new split
        magit-display-buffer-function (lambda (buffer)
                                        (display-buffer buffer '(display-buffer-same-window)))
        magit-section-initial-visibility-alist
        '((untracked . show)
          (unstaged . show)
          (unpushed . show)
          (unpulled . show)
          (stashes . show)))
  :bind (("C-c g s" . magit-status)
         ("C-c g b" . magit-blame))
  :config
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes unstage-all-changes))

  (add-hook 'git-commit-mode-hook 'init-git-commit-mode)

  ;; Real dates, please
  (set-default 'magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t))

(use-package magit-todos
  :ensure t
  :commands (magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 100))

(use-package forge
  :ensure t
  :config
  (setq forge-topic-list-limit '(10 . -1))
  :bind
  ("C-c f r" . forge-list-requested-reviews)
  ("C-c f p" . forge-list-authored-pullreqs)
  ("C-c f c" . forge-create-pullreq))

(use-package git-timemachine
  :ensure t)

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

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode +1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

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
  (global-set-key (kbd "H-s-t") 'tgt-toggle))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-checker-error-threshold 4000))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package flycheck-projectile
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode +1))

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
   ("C-c t" . yas-describe-tables)))

(use-package yasnippet-snippets
  :ensure t)

;;; use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(use-package quickrun :ensure t)

;;; Lisps

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'git-commit-mode-hook 'enable-paredit-mode)
  (add-hook 'racket-mode-hook 'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook 'enable-paredit-mode)
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

;;; Python

(defun bk/elpy-setup ()
  "Setup python mode."
  (pyvenv-activate "~/miniconda3")
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(use-package elpy
  :ensure t
  :config
  (add-hook 'python-mode-hook #'elpy-enable)
  (add-hook 'python-mode-hook #'bk/elpy-setup))

(use-package py-autopep8
  :ensure t
  :after elpy
  :init
  (setq py-autopep8-options '("--max-line-length=100"))
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;;; Java

(use-package lsp-mode
  :ensure t
  :hook ((java-mode . #'lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-intelephense-multi-root nil
        lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-completion-provider :capf
        lsp-idle-delay 0.500))

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug))
  :hook ((dap-mode . dap-ui-mode)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-delay 1.5
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-width 100))

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp))

(defun bk/setup-java ()
  "Setup JAVA."
  (electric-pair-mode +1))

(add-hook 'java-mode-hook 'bk/setup-java)

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

(use-package subword
  :diminish subword-mode
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package clojure-mode
  :ensure t
  :init
  (setq clojure-toplevel-inside-comment-form t)
  :config
  (require 'flycheck-clj-kondo))

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
  (eldoc-mode -1)
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

(use-package js2-mode
  :ensure t
  :init
  (setq js2-mode-show-parse-errors nil
        js2-missing-semi-one-line-override nil)
  :config
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode +1))))

(use-package js2-refactor
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

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

(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (define-key flyspell-mode-map (kbd "C-.") nil))


(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (require 'flyspell-correct-ido)
  (setq flyspell-correct-interface #'flyspell-correct-ido)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))


;;; Spell Checking

(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar
        "~/.emacs.d/bin/languagetool-commandline.jar"))

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

;; edit blocks
(use-package edit-indirect
  :ensure t)

(use-package org-download
  :ensure t
  :config
  (setq org-image-actual-width nil))

;; disable flycheck in org buffers
(defun disable-flycheck-in-org-src-block ()
  "Disable flycheck inside ORG src blocks."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)

;;; Zettelkasten

(defvar bk-zettelkasten-dir "/home/wanderson/zettelkasten")

(defun bk/roam-add-resource ()
  "Add resource to Org Roam buffer."
  (interactive)
  (let* ((candidates (mapcar (lambda (x)
                               (replace-regexp-in-string "~/resources/" "" x))
                             (directory-files-recursively "~/resources" "")))
         (resource (ido-completing-read "Resource: " candidates))
         (link-location (concat "file:~/resources/" resource))
         (description (ido-completing-read "Description: " "")))
    (org-insert-link t link-location description)))

(use-package org-roam
  :ensure t
  :diminish org-roam-mode
  :init
  (setq org-roam-directory bk-zettelkasten-dir
        org-roam-completion-everywhere t
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

(defun bk/clean-leader-on-save ()
  "Hook to keep the file organized."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

(defun bk/encoded-date (date)
  "Create DATE."
  (string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" date)
  (let* ((fixed-date
          (concat (match-string 1 date) "-"
                  (match-string 2 date) "-"
                  (match-string 3 date)))
         (d (parse-time-string fixed-date)))
    (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun bk/ledger-change-date (num)
  "Change the date based on NUM of days."
  (save-excursion
    (ledger-navigate-beginning-of-xact)
    (let* ((beg (point))
           (end (re-search-forward ledger-iso-date-regexp))
           (xact-date (filter-buffer-substring beg end)))
      (delete-region beg end)
      (insert
       (format-time-string
        "%Y/%m/%d"
        (time-add (bk/encoded-date xact-date)
                  (days-to-time num)))))))

(defun bk/ledger-increment-date ()
  "Increase the date number."
  (interactive)
  (bk/ledger-change-date 1))


(defun bk/ledger-decrement-date ()
  "Decrease the date number."
  (interactive)
  (bk/ledger-change-date -1))


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
  (setq ledger-report-auto-width t)
  (add-hook 'before-save-hook 'bk/clean-leader-on-save)
  :bind (:map ledger-mode-map
              ("C-M-." . bk/ledger-increment-date)
              ("C-M-," . bk/ledger-decrement-date)))

(use-package hledger-mode
  :ensure t
  :bind (:map hledger-mode-map
              ("TAB" . completion-at-point)))

;;; Docker


(defun bk/dockerfile-add-build-args ()
  "Add env variables to your docker build."
  (interactive)
  (let* ((vars (read-from-minibuffer "sequence of <envName>=<envValue>: "))
         (split-vars (split-string vars " ")))
    (setq dockerfile-build-args nil)
    (dolist (v split-vars)
      (add-to-list 'dockerfile-build-args v))
    (setq docker-build-history-args vars)))


(defun bk/docker-compose-custom-envs ()
  "Add usual env variables to Emacs environment."
  (interactive)
  (let* ((idu (shell-command-to-string "id -u"))
         (idg (shell-command-to-string "id -g"))
         (uid (string-join (vector (string-trim idu) ":" (string-trim idg)))))
    (setenv "WEBSERVER_PORT" "3000")
    (setenv "CURRENT_UID" uid)
    (message "setenv WEBSERVER_PORT=3000 CURRENT_UID=$(id -u):$(id -g) done!")))

(defun bk/docker-cleanup-buffers ()
  "Delete all the docker buffers created."
  (interactive)
  (kill-matching-buffers "docker" nil t))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile" . dockerfile-mode)
  :config
  (put 'dockerfile-image-name 'safe-local-variable-p #'stringp))

(use-package docker-compose-mode
  :ensure t)

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

;; reformat SQL using external program pgformatter
(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter)
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))


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

;; remap M-s .  because M-s was taken by paredit. Similar to vim * command
(global-set-key (kbd "C-c .") 'isearch-forward-symbol-at-point)


;;; nixOS

(use-package nix-mode
  :ensure t)

;;; Reify Health

(defun reifyhealth/cider-connect ()
  "Connect into eSource."
  (interactive)
  (cider-connect-clj (list :host "localhost" :port 12344)))

(defun reifyhealth ()
  "Open file notes from work."
  (interactive)
  (find-file "~/repos/reifyhealth/work.org"))

;;; Work in General

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
  (require 'pdf-continuous-scroll-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
  (require 'pdf-occur))


;;; Racket

(use-package racket-mode
  :ensure t
  :bind (:map racket-mode-map
              ("C-c C-k" . racket-run))
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))


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

;;; Telegram

(use-package alert
  :ensure t)

(use-package all-the-icons :ensure t)

(use-package company
  :ensure t)

(use-package telega
  :ensure t
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

;;; IIRC

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

(use-package erc
  :commands (erc)
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
  )

(require 'erc-compat)
(require 'erc-nicklist)
(setq erc-nicklist-icons-directory "~/.emacs.d/images/")

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

;;; Presentation

(use-package keycast
  :ensure t)

(use-package gif-screencast
  :ensure nil
  :load-path "~/.emacs.d/lisps/gif-screencast.el"
  :config
  (setq gif-screencast-program "maim"
        gif-screencast-args '("--quality" "1")
        gifgif-screencast-want-optimized t
        gif-screencast-output-directory "~/Videos/emacs/"
        gif-screencast-screenshot-directory "~/.emacs.d/screenshots"))

;;; EPUB

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))


;;; Webpaste

(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("ix.io" "dpaste.org")))

;;; End of file

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved cl-functions)
;; End:

;;; init.el ends here
