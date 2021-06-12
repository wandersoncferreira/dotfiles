;;; setup-clojure --- Changing Emacs clojure
;;
;;; Commentary:
;;
;;; Code:


;; * Functions

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


(defun bk/reload-cider-completion ()
  "Function to reload cider completion.
Better naming to improve the chances to find it."
  (interactive)
  (cider-completion-flush-caches))

;; * External Dependencies

(use-package clojure-snippets :ensure t)

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
  (require 'flycheck-clj-kondo)
  
  ;; config bc of guardrails library
  (put-clojure-indent '>defn :defn)
  (put-clojure-indent '>def :def)
  
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

(provide 'setup-clojure)
;;; setup-clojure.el ends here
