;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq
 user-full-name "Wanderson Ferreira"
 user-mail-address "wand@hey.com"
 tab-always-indent 'complete
 confirm-kill-emacs nil
 indent-tabs-mode nil
 byte-compile-warnings '(cl-functions)
 enable-local-variables t

 ;; auth
 auth-source-debug t
 auth-sources '((:source "~/.secrets/authinfo.gpg"))

 ;; appearance
 doom-theme 'doom-dracula
 doom-font "IBM Plex Mono 11"
 doom-scratch-initial-major-mode 'lisp-interaction-mode
 display-line-numbers-type nil

 ;; org
 org-directory "~/org/")

;; fullscreen from beginning
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove hooks
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; completion
;; no one needs completion *all* the time :(
(setq company-idle-delay nil)

;; take care of parenthesis
(use-package! paredit
  :hook ((clojure-mode . enable-paredit-mode)
         (git-commit-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)))


;; shift arrow to move between buffers
(windmove-default-keybindings)

;; lsp mode
(setq
 lsp-enable-file-watchers t
 lsp-semantic-tokens-enable t
 lsp-ui-sideline-enable nil
 lsp-ui-doc-enable nil
 lsp-enable-symbol-highlighting nil
 lsp-headerline-breadcrumb-enable t
 lsp-idle-delay 0.3)

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
(setq tgt-open-in-new-window nil)
(put 'tgt-projects 'safe-local-variable #'lisp)

;; magit
(setq magit-log-show-gpg-status t
      magit-commit-show-diff nil
      magit-refresh-status-buffer nil
      magit-display-buffer-function (lambda (buf)
                                      (display-buffer buf '(display-buffer-same-window))))

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

;; remove rainbow delimiters
(remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(setq
 cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources"
                       "~/Downloads/jvm11/source")
 cider-font-lock-dynamically '(macro core function var)
 cider-ns-refresh-show-log-buffer t
 cider-show-error-buffer t
 cider-save-file-on-load t)

(set-popup-rule! "*cider-test-report*" :side 'right :width 0.5)
(set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

(load! "+extra-cider")

(set-lookup-handlers! 'cider-mode nil)
(set-lookup-handlers! 'clj-refactor-mode nil)

(setq
 cljr-warn-on-eval nil
 cljr-eagerly-build-asts-on-startup nil
 cljr-add-ns-to-blank-clj-files nil)

;; java
(setq
 lsp-java-format-settings-profile "GoogleStyle"
 lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
 lsp-java-save-actions-organize-imports t)


(add-hook 'java-mode-hook #'electric-pair-mode)

;; org
(setq org-return-follows-link t
      org-confirm-babel-evaluate nil
      org-agenda-files (list "~/agenda/todo.org"))

;; zettelkasten
(setq org-roam-directory "/home/wanderson/zettelkasten")

;; finance
(use-package! ledger
  :mode ("\\ledger\\'" . ledger-mode)
  :config
  (load! "+extra-ledger"))

;; keybindigns
(load! "+extra-bindings")

;; work
(load! "+work-reifyhealth")
(load! "+work-appsauce")
