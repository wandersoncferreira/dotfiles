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
(set-register ?t '(file . "/home/wanderson/agenda/todo.org"))
(set-register ?l '(file . "/home/wanderson/ledger"))
(set-register ?e '(file . "/home/wanderson/.emacs.d/init.el"))

(defun bk/eval-buffer ()
  "Provide some feedback after evaluating the buffer."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))


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
	 ("<f5>" . bk/eval-buffer)))

(use-package ibuffer-vc
  :ensure t
  :bind
  (:map ibuffer-mode-map
	("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

(use-package vlf :ensure t)

(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode)

(load custom-file)

(windmove-default-keybindings)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(global-display-line-numbers-mode +1)

(delete-selection-mode +1)

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode +1))

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
  :diminish eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
	eldoc-echo-area-use-multiline-p nil)
  :config
  (global-eldoc-mode +1))

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
        ("C-a" .  bk/dired-back-to-start-of-files))
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

;;; bookmark
(use-package bm
  :ensure t
  :custom-face
  (bm-persistent-face ((t (:background "khaki2"))))
  :init
  (setq bm-restore-repository-on-load t
	bm-repository-file "~/.emacs.d/bm-repository"
	bm-buffer-persistence t
	bm-cycle-all-buffers t)
  :bind
  (("C-c b n" . bm-next)
   ("C-c b p" . bm-previous)
   ("C-c b b" . bm-toggle)))

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

(defun bk/fira-code-font (font-size)
  "Set the desired FONT-SIZE for Fira."
  (when (member "Fira Code" (font-family-list))
    (set-face-attribute 'default nil :font "Fira Code" :height font-size)))

(use-package simple
  :custom-face
  (mode-line ((t (:background "grey75" :foreground "black"))))
  :config
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1")
  (bk/fira-code-font 100))

(use-package simple
  :disabled t
  :config
  (load-theme 'default-black t)
  (bk/fira-code-font 100))

(use-package zenburn-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'zenburn t)
  (bk/set-consolas-font 110))

(use-package doom-themes
  :ensure t
  :disabled t
  :config
  (load-theme 'doom-dark+ t)
  (bk/fira-code-font 110))

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
         ("C-c <" . mc/mark-previous-like-this)
	 ("<mouse-3>" . mc/toggle-cursor-on-click)))

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
  :bind (("C-c g s" . magit-status)
	 ("C-c g b" . magit-blame)))

(use-package forge
  :ensure t
  :config
  (setq forge-topic-list-limit '(10 . -1))
  :bind
  ("C-c f r" . forge-list-requested-reviews)
  ("C-c f p" . forge-list-authored-pullreqs)
  ("C-c f c" . forge-create-pullreq))

(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'left)
  :custom-face
  (diff-hl-change ((t (:background "#3a81c3"))))
  (diff-hl-insert ((t (:background "#7ccd7c"))))
  (diff-hl-delete ((t (:background "#ee6363"))))
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

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

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-show-numbers t
        company-minimum-prefix-length 1
	company-format-margin-function #'company-vscode-light-icons-margin
	company-idle-delay 0.25)
  :config
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  (setq company-quickhelp-delay nil)
  :config
  (company-quickhelp-mode 1)
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

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
  :config
  (setq flycheck-check-syntax-automatically '(save)
	flycheck-checker-error-threshold 4000))

(use-package flycheck-projectile
  :ensure t)

(use-package popwin
  :ensure t
  :config
  (popwin-mode +1)
  (add-to-list 'popwin:special-display-config
	       `(,flycheck-projectile-error-list-buffer
		 :regexp nil :dedicated t :position bottom :stick t
		 :noselect nil)))

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

;;; symbol focus
(use-package symbol-focus
  :load-path "~/.emacs.d/lisps"
  :bind
  ("C-c f f" . sf/focus-at-point)
  :config
  (symbol-focus-mode +1))

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

(use-package toggle-test
  :ensure t
  :init
  (setq tgt-open-in-new-window nil)
  :config
  (put 'tgt-projects 'safe-local-variable #'lisp)
  (global-set-key (kbd "s-t") 'tgt-toggle))

(defvar bk--toggle-docs-clj nil)

(defun bk/toggle-docs-clj ()
  "Show docs."
  (interactive)
  (if bk--toggle-docs-clj
      (progn
	(setq bk--toggle-docs-clj nil)
	(lsp-ui-doc-hide))
    (progn
      (setq bk--toggle-docs-clj t)
      (lsp-ui-doc-show))))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-enable-file-watchers nil
	lsp-signature-render-documentation nil
	
        lsp-enable-indentation t
        lsp-completion-enable t
	lsp-lens-enable nil
	lsp-eldoc-enable-hover nil

	lsp-ui-doc-enable nil
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-include-signature t
	lsp-ui-doc-header t

	lsp-ui-sideline-enable t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-code-actions nil
	lsp-ui-sideline-show-hover nil
	lsp-ui-sideline-show-symbol nil
	
	lsp-headerline-breadcrumb-enable t
	lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-intelephense-multi-root nil

	lsp-modeline-code-actions-enable nil
	lsp-modeline-diagnostics-enable nil
        )
  :hook ((clojure-mode . lsp)
	 (clojurescript-mode . lsp)
	 (clojurec-mode . lsp)
	 (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-command-map (kbd "ra") 'lsp-clojure-add-missing-libspec)
  (define-key lsp-command-map (kbd "pr") 'lsp-ui-peek-find-references)
  (define-key lsp-command-map (kbd "pd") 'lsp-ui-peek-find-definitions)
  (define-key lsp-command-map (kbd "pi") 'lsp-ui-peek-find-implementation)
  (define-key lsp-command-map (kbd "pw") 'lsp-ui-peek-find-workspace-symbol)
  (define-key lsp-command-map (kbd "d") 'bk/toggle-docs-clj)
  (require 'lsp-ido))

(use-package cider
  :ensure t
  :init
  (setq cider-save-file-on-load t
        cider-print-options
        '(("length" 80)
          ("level" 20)
          ("right-margin" 80)))
  :config
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package clj-refactor
  :ensure t
  :init
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-favor-prefix-notation nil
	cljr-favor-private-functions nil
        cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("set" . "clojure.set")
          ("str" . "clojure.string")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("time" . "clj-time.core")
          ("log" . "clojure.tools.logging")
          ("json" . "cheshire.core")))
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'cider-mode-hook 'clj-refactor-mode)

  ;; refactor functionalities available in clojure mode will be added here too
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "aa") 'clojure-add-arity)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "c!") 'clojure-cycle-not)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "cw") 'clojure-cycle-when)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "ra") 'clojure-rename-ns-alias)

  ;; I would like to use lsp-mode for features that clj-refactor require ASTs evaluation
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "fu") 'lsp-ui-peek-find-references)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "fr") 'lsp-find-references)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "rs") 'lsp-rename)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "ef") 'lsp-clojure-extract-function)
  (define-key clj-refactor-map (cljr--key-pairs-with-prefix "C-c C-m" "is") 'lsp-clojure-inline-symbol)
  ;; missing:
  ;; promote-function
  ;; rename-file-or-dir
  )


;; Experimental configuration to hotload refactor
;; using Pomegranate from Cemerick
;; From https://www.eigenbahn.com/2020/05/06/fast-clojure-deps-auto-reload
;; and integrated into clj-refactor.

(eval-after-load 'cider
  '(progn
     (defun bk/send-to-repl (sexp eval ns)
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

(eval-after-load 'cider
  (setq lsp-enable-xref nil))

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
(defvar bk-zettelkasten-dir "/home/wanderson/zettelkasten")

(use-package org
  :ensure nil
  :init
  (setq org-return-follows-link t
	org-confirm-babel-evaluate nil
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

;;; docker
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

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4)
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode)))

(use-package sql-indent
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

;;; ripgrep
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

;;; nixOS

(use-package nix-mode :ensure t)
(use-package nixpkgs-fmt :ensure t)
(use-package nix-sandbox :ensure t)
(use-package nix-buffer :ensure t)
(use-package nixos-options :ensure t)

(use-package company-nixos-options
  :ensure t
  :config
  (with-eval-after-load 'company-nixos-options
    (defun company-nixos--in-nix-context-p ()
      (unless (executable-find "nix-build")
	(or (derived-mode-p 'nix-mode 'nix-repl-mode)
	    (let ((file-name (buffer-file-name (current-buffer))))
	      (and file-name (equal "nix" (file-name-extension file-name))))))))
  (add-to-list 'company-backends 'company-nixos-options))

;;; wgrep
(use-package wgrep :ensure t)

;;; pomodor
(use-package pomidor
  :ensure t
  :bind
  ("<f12>" . pomidor)
  :config
  (setq pomidor-sound-tick nil
	pomidor-sound-tack nil))


;;; reify health

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
  (end-of-buffer)
  (org-insert-heading-respect-content)
  (org-metaright)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
  (org-metaleft)
  (reindent-then-newline-and-indent)
  (reindent-then-newline-and-indent)
  (org-cycle)
  (insert "- "))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here