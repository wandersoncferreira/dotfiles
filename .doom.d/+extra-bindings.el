;;; +extra-bindings.el -*- lexical-binding: t; -*-

(setq which-key-idle-delay 0.4
      tab-always-indent 'complete)

(set-register ?l '(file . "/Users/wferreir/ledger"))
(set-register ?b '(file . "/Users/wferreir/dotfiles/macos/Brewfile"))

(map! "C-x C-m" #'execute-extended-command

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

      ;; pomidor
      "<f12>" #'pomidor

      ;; tests
      "H-t" #'projectile-toggle-between-implementation-and-test
      )

;; clojure
(map! :map clojure-mode-map

      ;; tests
      "C-c k t" #'kaocha-runner-run-test-at-point
      "C-c k r" #'kaocha-runner-run-tests
      "C-c k a" #'kaocha-runner-run-all-tests
      "C-c k w" #'kaocha-runner-show-warnings
      "C-c k h" #'kaocha-runner-hide-windows
      )
