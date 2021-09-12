;;; ../dotfiles/.doom.d/+extra-clojure.el -*- lexical-binding: t; -*-


(after! clojure-mode
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook! 'clojure-mode-hook (enable-paredit-mode))
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

  (when IS-MAC
    (setq lsp-clojure-server-command "/opt/homebrew/bin/clojure-lsp")))

(defun bk/clojure-pid ()
  "Find PID of current clojure process."
  (interactive)
  (cider-repl-set-ns "user")
  (message
   (cider-nrepl-sync-request:eval
    "(-> (java.lang.management.ManagementFactory/getRuntimeMXBean)
    (.getName)
    (clojure.string/split #\"@\")
    (first)")))


(after! cider-mode
  (setq cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources" "~/Downloads/jvm11/source")
        cider-show-error-buffer t
        cider-save-file-on-load t
        cider-eldoc-display-for-symbol-at-point nil
        clojure-toplevel-inside-comment-form t
        cider-clojure-cli-command "/Users/wferreir/teste.sh")

  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

  (add-hook! 'cider-test-report-mode-hook 'toggle-truncate-lines)
  (load! "+patch-cider"))
