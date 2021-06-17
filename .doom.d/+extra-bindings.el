;;; +extra-bindings.el -*- lexical-binding: t; -*-

(set-register ?l '(file . "/home/wanderson/ledger"))

(map! "C-x C-m" #'counsel-M-x

      ;; editor
      "M-p" #'jump-char-backward
      "M-n" #'jump-char-forward
      "M-i" #'change-inner
      "M-u" #'fix-word-upcase
      "M-l" #'fix-word-downcase
      "M-c" #'fix-word-capitalize

      ;; window
      "C-x 3" #'bk/hsplit-last-buffer
      "C-x 2" #'bk/vsplit-last-buffer

      ;; movement
      "C-x p" #'pop-to-mark-command

      ;; completion
      "C-." #'completion-at-point
      )

;; clojure

(map! :map clojure-mode-map

      ;; kaocha
      "C-c k t" #'kaocha-runner-run-test-at-point
      "C-c k r" #'kaocha-runner-run-tests
      "C-c k a" #'kaocha-runner-run-all-tests
      "C-c k w" #'kaocha-runner-show-warnings
      "C-c k h" #'kaocha-runner-hide-windows
      "H-s-t" #'tgt-toggle

      ;; def
      "M-." #'find-definition

      :map cider-mode-map
      "M-." #'find-definition

      :map clojurec-mode-map
      "M-." #'find-definition

      :map clojurescript-mode-map
      "M-." #'find-definition)
