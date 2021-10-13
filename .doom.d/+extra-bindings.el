;;; +extra-bindings.el -*- lexical-binding: t; -*-

(setq which-key-idle-delay 0.4
      tab-always-indent 'complete)

(set-register ?l '(file . "/Users/wferreir/ledger"))
(set-register ?b '(file . "/Users/wferreir/dotfiles/macos/Brewfile"))

(map! "C-x C-m" #'execute-extended-command

      "C-x C-j" #'dired-jump

      ;; buffers
      "C-c c SPC" #'rotate-layout
      "C-x b" #'+ivy/switch-workspace-buffer
      "C-c b" #'+ivy/switch-workspace-buffer-other-window
      "C-x C-b" #'+ivy/switch-buffer
      "C-c C-b" #'+ivy/switch-buffer-other-window

      ;; screencast
      "<f9>" #'gif-screencast-start-or-stop

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
      "C-:" #'avy-goto-char
      "M-g w" #'avy-goto-word-1

      ;; completion
      "C-." #'completion-at-point

      ;; pomidor
      "<f12>" #'pomidor

      ;; tests
      "s-t" #'projectile-toggle-between-implementation-and-test
      )

;; undo - map a separate "redo" action to Emacs
(after! undo-fu
  (map! :map undo-fu-mode-map
        "C-?" #'undo-fu-only-redo))

;; clojure
(map! :map clojure-mode-map
      ;; tests
      "C-c k t" #'kaocha-runner-run-test-at-point
      "C-c k r" #'kaocha-runner-run-tests
      "C-c k a" #'kaocha-runner-run-all-tests
      "C-c k w" #'kaocha-runner-show-warnings
      "C-c k h" #'kaocha-runner-hide-windows)

;; useful functions
(after! prog-mode
  (map! :map prog-mode-map "C-h C-f" #'find-function-at-point)
  (map! :map prog-mode-map
        :localleader
        :desc "Find function at point"
        "g p" #'find-function-at-point))


;; symbol focus bindings
(map! :map symbol-focus-mode-map
      "C-s-b" #'sf/back
      "C-s-n" #'sf/next
      "C-s-p" #'sf/prev
      "C-s-r" #'sf/reset)
