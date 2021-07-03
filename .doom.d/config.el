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
      doom-font (font-spec :family "Hasklig" :size 14)
      doom-variable-pitch-font (font-spec :family "Hasklig" :size 14)
      display-line-numbers-type nil)


;; delete selection
(delete-selection-mode +1)

;; fullscreen from beginning
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove hooks
(remove-hook 'doom-first-buffer-hook 'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook 'smartparens-global-mode)
(remove-hook 'org-mode-hook #'org-superstar-mode)

(after! clojure-mode
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook! 'clojure-mode-hook (enable-paredit-mode))
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(after! elisp-mode
  (add-hook! 'emacs-lisp-mode-hook (enable-paredit-mode))
  (remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; appearance

(set-face-attribute 'lazy-highlight nil :background "khaki1")
(set-face-attribute 'isearch nil :background "khaki1")
(set-face-attribute 'region nil :background "khaki1")

;; company
(after! company
  (setq company-idle-delay 0.2))

;; shift arrow to move between buffers
(windmove-default-keybindings)

;; lsp mode
(after! lsp-mode
  (setq lsp-enable-file-watchers t
        lsp-ui-sideline-show-code-actions nil
        lsp-enable-symbol-highlighting t
        lsp-lens-enable nil
        lsp-eldoc-enable-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-headerline-breadcrumb-enable nil
        lsp-idle-delay 0.2)
  (add-to-list 'lsp-file-watch-ignored-directories "classes")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\terraform\\'"))

(advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))

;; typescript
(add-hook 'typescript-mode-hook #'format-all-mode)
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)

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
  (setq cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources" "~/Downloads/jvm11/source")
        cider-show-error-buffer t
        cider-save-file-on-load t
        cider-eldoc-display-for-symbol-at-point nil)

  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

  (add-hook! 'cider-test-report-mode-hook 'toggle-truncate-lines)
  (load! "+extra-cider"))

(after! clj-refactor
  (setq cljr-add-ns-to-blank-clj-files nil))

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
        org-directory "~/org/"
        org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil
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
