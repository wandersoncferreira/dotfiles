;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wand@hey.com"
      tab-always-indent 'complete
      confirm-kill-emacs nil
      byte-compile-warnings '(cl-functions)
      enable-local-variables t

      ;; spell
      ispell-dictionary "en"

      ;; editor
      fill-column 180
      indent-tabs-mode nil

      ;; auth
      auth-source-debug t
      auth-sources '((:source "~/.secrets/authinfo.gpg"))

      ;; appearance
      doom-theme 'nil
      display-line-numbers-type nil

      ;; org
      org-directory "~/org/")

;; delete selection
(delete-selection-mode +1)

;; fullscreen from beginning
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove hooks
(remove-hook 'doom-first-buffer-hook 'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook 'smartparens-global-mode)

(after! clojure-mode
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(after! elisp-mode
  (remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; appearance
(set-face-attribute 'lazy-highlight nil :background "khaki1")
(set-face-attribute 'isearch nil :background "khaki1")
(set-face-attribute 'region nil :background "khaki1")

;; completion
;; no one needs completion *all* the time :(
(after! company
  (setq company-idle-delay 0.3
        company-tooltip-align-annotations t))

;; take care of parenthesis
(after! paredit
  (add-hook! 'clojure-mode-hook (enable-paredit-mode))
  (add-hook! 'git-commit-mode-hook (enable-paredit-mode))
  (add-hook! 'emacs-lisp-mode-hook (enable-paredit-mode)))

;; shift arrow to move between buffers
(windmove-default-keybindings)

;; lsp mode
(after! lsp-mode
  (setq lsp-enable-file-watchers t
        lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.3)

  (add-to-list 'lsp-file-watch-ignored-directories "classes")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'"))

(advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))

;; window
(load! "+extra-window")

;; eshell
(defun eshell-clear-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;; auth
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

;; test
(after! toggle-test
  (setq tgt-open-in-new-window nil)
  (put 'tgt-projects 'safe-local-variable #'lisp))

;; magit
(after! magit-mode
  (setq magit-log-show-gpg-status t
        magit-commit-show-diff nil
        magit-display-buffer-function (lambda (buf) (display-buffer buf '(display-buffer-same-window)))))

;; clojure
(defun find-refs ()
  (interactive)
  (lsp-find-references t))

(defun find-definition ()
  "Try to find definition of cursor via LSP otherwise fallback to cider."
  (interactive)
  (let ((cursor (point))
        (buffer (current-buffer)))
    (lsp-find-definition)
    (when (and (eq buffer (current-buffer))
               (eq cursor (point)))
      (cider-find-var))))

(after! cider-mode
  (setq cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources"
                              "~/Downloads/jvm11/source")
        cider-save-file-on-load t)

  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

  (add-hook! 'cider-test-report-mode-hook (toggle-truncate-lines +1))

  (load! "+extra-cider"))

(after! clj-refactor
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil))

;; java
(after! lsp-java
  (setq lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-save-actions-organize-imports t)

  (add-hook! 'java-mode-hook (electric-pair-mode +1))
  (add-hook! 'java-mode-hook (subword-mode +1)))

(after! cc-mode
  (remove-hook 'java-mode-hook #'rainbow-delimiters-mode))

;; org
(after! org
  (setq org-return-follows-link t
        org-confirm-babel-evaluate nil
        org-agenda-files (list "~/agenda/todo.org")))

(after! org-roam-server
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 17042
        org-roam-server-export-inline-images t)
  (require 'org-roam-protocol))

;; zettelkasten
(after! org-roam
  (setq org-roam-directory "/home/wanderson/zettelkasten"))

;; finance
(add-to-list 'auto-mode-alist '("\\ledger\\'" . ledger-mode))

(after! ledger-mode
  (load! "+extra-ledger"))

;; keybindigns
(load! "+extra-bindings")

;; work
(load! "+work-reifyhealth")
(load! "+work-appsauce")
