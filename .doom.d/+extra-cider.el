;;; +extra-cider.el -*- lexical-binding: t; -*-

;; Experimental configuration to hotload refactor
;; using Pomegranate from Cemerick
;; From https://www.eigenbahn.com/2020/05/06/fast-clojure-deps-auto-reload
;; and integrated into clj-refactor.

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
      (cljr-hotload-dependency artifact version))))
