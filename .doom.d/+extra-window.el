;;; +extra-window.el -*- lexical-binding: t; -*-

;; shift arrow to move between buffers
(windmove-default-keybindings)

(defun bk/vsplit-last-buffer ()
  "Split the window vertically and display the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun bk/hsplit-last-buffer ()
  "Split the window horizontally and display the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

;; list of buffers produced by `counsel-buffer-or-recentf'
;; is shown in the order the buffers have been visited.
(use-package! switch-buffer-functions
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))

;; tame the buffers!
(use-package! popper
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          ("\\*Async Shell Command\\*" . hide)
          help-mode
          (eshell-mode . hide)
          cider-repl-mode)
        popper-group-function #'popper-group-by-projectile)
  :config
  (popper-mode +1)
  (popper-echo-mode +1))
