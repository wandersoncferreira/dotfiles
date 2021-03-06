;;; +extra-bindings.el -*- lexical-binding: t; -*-

(setq which-key-idle-delay 0.4
      tab-always-indent 'complete)

(set-register ?l '(file . "/home/wanderson/ledger"))
(set-register ?b '(file . "/Users/wferreir/dotfiles/macos/Brewfile"))

(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier nil))

(map! "C-x C-m" #'counsel-M-x

      "C-x C-j" #'dired-jump

      ;; editor
      "M-p" #'jump-char-backward
      "M-n" #'jump-char-forward
      "M-i" #'change-inner
      "M-u" #'fix-word-upcase
      "M-l" #'fix-word-downcase
      "M-c" #'fix-word-capitalize
      "C-<up>" #'move-text-up
      "C-<down>" #'move-text-down
      "C-c d" #'crux-duplicate-current-line-or-region

      ;; window
      "C-x 3" #'bk/hsplit-last-buffer
      "C-x 2" #'bk/vsplit-last-buffer
      "C-x k" #'kill-this-buffer

      ;; movement
      "C-x p" #'pop-to-mark-command

      ;; completion
      "C-." #'completion-at-point

      )

;; clojure
(map! :map clojure-mode-map

      ;; tests
      "C-c k t" #'kaocha-runner-run-test-at-point
      "C-c k r" #'kaocha-runner-run-tests
      "C-c k a" #'kaocha-runner-run-all-tests
      "C-c k w" #'kaocha-runner-show-warnings
      "C-c k h" #'kaocha-runner-hide-windows
      "H-s-t" #'projectile-toggle-between-implementation-and-test

      ;; definition
      "M-." #'find-definition

      :map cider-mode-map
      "M-." #'find-definition

      :map clojurec-mode-map
      "M-." #'find-definition

      :map clojurescript-mode-map
      "M-." #'find-definition)
