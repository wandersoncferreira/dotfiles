;;; +extra-bindings.el -*- lexical-binding: t; -*-

(setq which-key-idle-delay 0.4
      tab-always-indent 'complete)

(set-register ?l '(file . "/Users/wferreir/ledger"))
(set-register ?b '(file . "/Users/wferreir/dotfiles/macos/Brewfile"))

(map! "C-x p" nil) ;; I really need pop-to-mark here

(map!
 ;; C-x keys
 "C-x 2" #'bk/vsplit-last-buffer
 "C-x 3" #'bk/hsplit-last-buffer
 "C-x b" #'+ivy/switch-workspace-buffer
 "C-x p" #'pop-to-mark-command
 "C-x k" #'kill-this-buffer
 "C-x C-b" #'+ivy/switch-buffer
 "C-x C-m" #'execute-extended-command
 "C-x C-j" #'dired-jump

 ;; super keys
 "s-t" #'projectile-toggle-between-implementation-and-test
 "s-'" #'cycle-quotes
 "s-s" #'deadgrep
 "s-g" #'gh-notify
 "s-p" #'+popup/toggle

 ;; F-* keys
 "<f5>" #'deadgrep
 "<f9>" #'gif-screencast-start-or-stop
 "<f12>" #'pomidor

 ;; C-c keys
 "C-c b" #'+ivy/switch-workspace-buffer-other-window
 "C-c d" #'crux-duplicate-current-line-or-region
 "C-c c SPC" #'rotate-layout
 "C-c C-b" #'+ivy/switch-buffer-other-window

 ;; editor
 "M-p" #'jump-char-backward
 "M-n" #'jump-char-forward
 "M-i" #'change-inner
 "M-u" #'fix-word-upcase
 "M-l" #'fix-word-downcase
 "M-c" #'fix-word-capitalize
 "C-<up>" #'move-text-up
 "C-<down>" #'move-text-down

 ;; movement
 "C-:" #'avy-goto-char
 "M-g w" #'avy-goto-word-1

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

;; vc functions
(map! :map forge-post-mode-map
      "C-c C-d" #'bk/post-draft-pull-request

      :map diff-mode-map
      "C-c s" #'bk/github-review--copy-suggestion

      :map forge-topic-mode-map
      "C-c r" #'github-review-forge-pr-at-point)

;; enable command
(put 'narrow-to-region 'disabled nil)
