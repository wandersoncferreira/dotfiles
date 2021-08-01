;;; ../dotfiles/.doom.d/+extra-clojure.el -*- lexical-binding: t; -*-


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


(after! clojure-mode
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook! 'clojure-mode-hook (enable-paredit-mode))
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

  (when IS-MAC
    (setq lsp-clojure-server-command "/opt/homebrew/bin/clojure-lsp")))


(after! cider-mode
  (setq cider-jdk-src-paths '("~/Downloads/clojure-1.10.3-sources" "~/Downloads/jvm11/source")
        cider-show-error-buffer t
        cider-save-file-on-load t
        cider-eldoc-display-for-symbol-at-point nil)

  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

  (add-hook! 'cider-test-report-mode-hook 'toggle-truncate-lines)
  (load! "+patch-cider"))
