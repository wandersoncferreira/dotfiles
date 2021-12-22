;;; +extra-bindings.el -*- lexical-binding: t; -*-

(setq which-key-idle-delay 0.4
      tab-always-indent 'complete)

(set-register ?l '(file . "/Users/wferreir/ledger"))
(set-register ?b '(file . "/Users/wferreir/dotfiles/macos/Brewfile"))

(map! "C-x p" nil) ;; I really need pop-to-mark here

(map!
 ;; C-x keys
 "C-x b" #'+vertico/switch-workspace-buffer
 "C-x p" #'pop-to-mark-command
 "C-x k" #'kill-this-buffer
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
 "C-c d" #'crux-duplicate-current-line-or-region
 "C-c c SPC" #'rotate-layout

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

;; enable command
(put 'narrow-to-region 'disabled nil)
