;;; ../dotfiles/.doom.d/+extra-clojure.el -*- lexical-binding: t; -*-

(use-package! clojure-mode
  :init
  (setq clojure-thread-all-but-last t)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook! 'clojure-mode-hook (enable-paredit-mode))
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(when IS-MAC
  (setq lsp-clojure-server-command "/opt/homebrew/bin/clojure-lsp"))

(defun bk/sync-eval-to-string (s)
  "Execute clojure code S and return the result as string"
  (let* ((x (concat "(do (clojure.core/in-ns '"
                    (cider-current-ns)
                    ") " s ")"))
         (dict (cider-nrepl-sync-request:eval x))
         (e (nrepl-dict-get dict "err"))
         (v (nrepl-dict-get dict "value")))
    (if e
        (user-error! e)
      v)))

(defun bk/clojure-pid ()
  "Find PID of current clojure process."
  (interactive)
  (message
   (bk/sync-eval-to-string
    "(-> (java.lang.management.ManagementFactory/getRuntimeMXBean)
         (.getName)
         (clojure.string/split #\"@\")
         (first))")))

;; portal integration

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "
(require 'portal.api)
(portal.api/open {:portal.colors/theme :portal.colors/solarized-light})
(portal.api/tap)
"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; reveal extensions

(defun bk/reveal.extensions ()
  "Install Sean Corfield's extensions to Reveal panel"
  (interactive)
  (cider-nrepl-sync-request:eval
   "(load-file (str (System/getProperty \"user.home\") \"/.clojure/dev.clj\"))"))

(use-package! cider
  :after clojure-mode
  :init
  (setq cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources" "~/Downloads/jvm11/source")
        cider-show-error-buffer t
        cider-save-file-on-load t
        cider-eldoc-display-for-symbol-at-point nil
        cider-repl-use-pretty-printing nil
        cider-redirect-server-output-to-repl t
        clojure-toplevel-inside-comment-form t
        cider-clojure-cli-command "/Users/wferreir/teste.sh"
        ;; cider-clojure-cli-aliases "portal"

        cljr-injected-middleware-version "3.0.0-alpha13")
  :config

  (add-hook! 'cider-test-report-mode-hook 'toggle-truncate-lines))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (setq cljr-warn-on-eval nil))

(after! lsp-mode
  (setq lsp-completion-enable nil
        lsp-enable-indentation nil))

;; run `dash-docs-install-docset' to get it if new installation
(set-docsets! 'clojure-mode "Clojure")
