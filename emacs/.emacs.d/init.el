;;; init.el --- Wand's config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-pinned-packages '())

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;;; my own patches
(setq lisps-dir (expand-file-name "lisps" user-emacs-directory))
(add-to-list 'load-path lisps-dir)

;;; register shortcuts
(set-register ?t '(file . "/home/wanderson/arch/agenda/todo.org"))
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
	    create-lockfiles nil
	    custom-safe-themes t
	    indent-tabs-mode nil
	    tab-width 4
	    make-backup-files nil
	    gc-cons-threshold (* 100 1024 1024)
	    read-process-output-max (* 4 1024 1024)
	    custom-file (expand-file-name "custom.el" user-emacs-directory))
  :bind (("C-x p" . pop-to-mark-command)
	     ("C-x C-b" . ibuffer)
	     ("C-x e" . eshell)
	     :map emacs-lisp-mode-map
	     ("<f5>" . bk/eval-buffer))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode)
  (load custom-file)
  (windmove-default-keybindings)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (display-line-numbers-mode t))

(use-package autorevert
  :diminish auto-revert-mode)

;;; auth
(use-package pinentry
  :ensure t
  :config
  (setq epg-gpg-program "gpg"
	    epa-file-cache-passphrase-for-symmetric-encryption t))

(use-package epa
  :ensure nil
  :init
  (setq epa-pinentry-mode 'loopback
	    auth-source-debug t
	    auth-sources '((:source "~/.secrets/authinfo.gpg")))
  :config
  (pinentry-start))

(defun bk/bitwarden ()
  "Get bitwarden."
  (interactive)
  (kill-new (auth-source-pick-first-password
             :host "bitwarden.app"
             :user "bartuka")))

;;; eshell

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

(use-package recentf
  :config
  (recentf-mode +1))

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

(use-package eldoc
  :diminish eldoc-mode)

(defun bk/dired-directories-first ()
  "Sorted dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(use-package dired
  :config
  (require 'dired-x)
  (setq dired-dwim-target t)
  (advice-add 'dired-readin :after #'bk/dired-directories-first))

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
	          :map ido-common-completion-map
	          ("SPC" . self-insert-command)
	          ("M-SPC" . just-one-space))
  :config
  (ido-mode +1)
  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules")
  (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	     ("C-x C-m" . smex)))

(use-package ido-completing-read+
  :ensure t
  :after ido
  :config
  (ido-ubiquitous-mode +1))

(defun bk/set-monaco-font (font-size)
  "Set the desired FONT-SIZE for Monaco."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height font-size)))

(defun bk/set-hack-font (font-size)
  "Set the desired FONT-SIZE for Hack."
  (when (member "Hack" (font-family-list))
    (set-face-attribute 'default nil :font "Hack" :height font-size)))

(defun bk/set-consolas-font (font-size)
  "Set the desired FONT-SIZE for Consolas."
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas" :height font-size)))

(use-package simple
  :custom-face
  (mode-line ((t (:background "grey75" :foreground "black"))))
  :config
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1")
  (bk/set-consolas-font 110))

;; (set-background-color "honeydew")
;; (set-face-attribute 'default nil :height 110)
;; (set-face-attribute 'lazy-highlight nil :background "khaki1")
;; (set-face-attribute 'isearch nil :background "khaki1")
;; (set-face-attribute 'region nil :background "khaki1")
;; (bk/set-hack-font 100)

;;; experiment with transparent sessions

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

;; projects
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ido
        projectile-dynamic-mode-line nil
        projectile-ignored-projects '("~/" "/tmp")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "TAGS" "ido.last" "recentf" "smex-items"))
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file)
	     :map projectile-mode-map
	     ("C-c p" . projectile-command-map))
  :config
  (require 'subr-x)
  (projectile-mode +1))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w"))

;;; programming docs
(use-package zeal-at-point
  :ensure t
  :bind
  ("C-c d d" . zeal-at-point))

;; editor
(use-package expand-region
  :ensure t
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

(use-package jump-char
  :ensure t
  :bind (("M-n" . jump-char-forward)
         ("M-p" . jump-char-backward)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

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

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))

(defun bk/duplicate-current-line-or-region (arg)
  "Duplicate the current line or region ARG times."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

(global-set-key (kbd "C-c d l") #'bk/duplicate-current-line-or-region)

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


;;; git

(use-package magit
  :ensure t
  :init
  (setq magit-diff-refine-hunk t)
  :bind ("C-c g s" . magit-status))

(use-package forge
  :ensure t
  :config
  (setq forge-topic-list-limit '(10 . -1))
  :bind
  ("C-c f r" . forge-list-requested-reviews)
  ("C-c f p" . forge-list-authored-pullreqs)
  ("C-c f c" . forge-create-pullreq))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-show-numbers t
	    company-idle-delay 0.25)
  :config
  (global-company-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package java-snippets :ensure t)
(use-package clojure-snippets :ensure t)
(use-package quickrun :ensure t)

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; lisps
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

;; clojure

(defun bk/nrepl-warn-when-not-connected ()
  "Function to warn me to start the REPL."
  (interactive)
  (message "Oops! You're not connected to an nREPL server.
Please run M-x cider or M-x cider-jack-in to connect"))

(use-package clojure-mode :ensure t)

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

(use-package cider :ensure t)

(use-package lsp-mode
  :ensure nil
  :load-path "/home/wanderson/work/code/opensource/lsp-mode"
  :init
  (setq lsp-keymap-prefix "C-c l"
	    lsp-enable-file-watchers nil
	    lsp-semantic-tokens-enable t
	    lsp-signature-render-documentation t
	    lsp-lens-enable nil
	    lsp-ui-sideline-show-code-actions nil
	    lsp-headerline-breadcrumb-enable nil
        lsp-intelephense-multi-root nil)
  :hook ((clojure-mode . lsp)
	     (clojurescript-mode . lsp)
	     (clojurec-mode . lsp)
	     (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-command-map (kbd "ra") 'lsp-clojure-add-missing-libspec)
  (require 'lsp-ido))

(eval-after-load 'projectile
  '(progn
     (pushnew! projectile-globally-ignored-directories ".cpcache")))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;;; java
(use-package lsp-java
  :ensure t
  :after lsp
  :hook ((java-mode . lsp))
  :config
  (add-hook 'java-mode-hook 'electric-pair-mode))

;; typescript

(defun bk/setup-lsp-typescript-mode ()
  "Setup js development."
  (interactive)
  (lsp)
  (eldoc-mode +1)
  (electric-pair-mode +1)
  (prettier-js-mode +1)
  (add-node-modules-path))

(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx\\'" . typescript-mode)
  :hook (typescript-mode . bk/setup-lsp-typescript-mode))

(use-package prettier-js :ensure t)
(use-package add-node-modules-path :ensure t)

(setq-default js-indent-level 2)

(use-package tide
  :ensure t
  :disabled 3
  :preface
  (defun bk/setup-tide-mode ()
    "Setup js development."
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (electric-pair-mode +1)
    (tide-hl-identifier-mode +1)
    (prettier-js-mode +1)
    (add-node-modules-path))
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

;; helpers
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; custom functions
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


;;; custom functions -- end --

;; spell checking
(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar
	    "~/.emacs.d/bin/languagetool-commandline.jar"))

(use-package popup :ensure t)

(use-package google-translate
  :ensure t
  :commands (google-translate-smooth-translate)
  :preface
  (defun bk/translate ()
    "Translate."
    (interactive)
    (google-translate-smooth-translate))
  :config
  (setq google-translate-backend-method 'curl
	    google-translate-translation-directions-alist '(("en" . "en")
							                            ("en" . "pt")))
  (defun google-translate--search-tkk ()
    "Fixing bug."
    (list 430675 2721866130)))

;; zettelkasten
(defvar bk-zettelkasten-dir "/home/wanderson/arch/zettelkasten")

(use-package org
  :ensure nil
  :init
  (setq org-return-follows-link t
	    org-confirm-babel-evaluate nil
	    org-src-fontify-natively t
	    org-src-tab-acts-natively t
	    org-agenda-files (list "~/arch/agenda/todo.org"))
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
	      ("t" "Todo" entry (file+headline "~/arch/agenda/todo.org" "Task")
	       "* TODO %^{Title}\n %i" :clock-in t :clock-resume t))
	    ))

(use-package org-download
  :ensure t
  :config
  (setq org-image-actual-width nil))

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
  (org-roam-mode +1))

(use-package org-roam-server
  :ensure t
  :commands (org-roam-server-mode)
  :init
  (setq org-roam-server-port 17042)
  :config
  (require 'server)
  (unless (server-running-p)
    (server-start))
  (require 'org-roam-protocol))

;;; uuids
(use-package uuidgen
  :preface
  (defun bk/uuid ()
    "Create uuid and add to clipboard."
    (interactive)
    (kill-new (uuidgen-4)))
  :ensure t)

;;; google
(use-package google-this
  :ensure t)

;;; finance
(use-package ledger-mode
  :ensure t
  :mode ("ledger" . ledger-mode)
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
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here
