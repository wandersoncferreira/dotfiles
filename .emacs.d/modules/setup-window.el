;;; setup-window --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:

;; * Attributes

(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator " * "
      uniquify-after-kill-buffer-p t
      uniquify-strip-common-suffix t
      uniquify-ignore-buffers-re "^\\*")

;; use arrow keys to move between window
(windmove-default-keybindings)

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t
        winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"
          "*kaocha-error*"))
  :config
  (winner-mode +1)
  (global-set-key (kbd "C-x 4 u") 'winner-undo)
  (global-set-key (kbd "C-x 4 U") 'winner-redo))

;; * External Dependencies

(use-package popup :ensure t)

(use-package windresize :ensure t)

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "M-s-<up>") 'buf-move-up)
  (global-set-key (kbd "M-s-<down>") 'buf-move-down)
  (global-set-key (kbd "M-s-<left>") 'buf-move-left)
  (global-set-key (kbd "M-s-<right>") 'buf-move-right))

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '(("*kaocha-error*" :ignore t)))
  (shackle-mode +1))

;; automatically group all of your Emacs buffers into workspaces by defining a series of
;; grouping rules. I find this a lot better than perspective-mode which I have to manually
;; add buffers to each workspace.

(use-package bufler
  :ensure t
  :bind
  (("C-c b" . bufler-switch-buffer)
   ("C-c s f" . bufler-workspace-frame-set))
  :config
  (bufler-mode +1)
  (setf bufler-groups
        (bufler-defgroups
          (group
           (auto-projectile))
          (auto-directory))))

(use-package zygospore
  :ensure t
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;; * Functions

(defun bk/toggle-window-split ()
  "Toggle window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun bk/kill-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun bk/indent-buffer ()
  "Fix indentation of buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun bk/untabify-buffer ()
  "Remove tabs from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun bk/touch-buffer-file ()
  "Touch buffer."
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

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

(provide 'setup-window)
;;; setup-window.el ends here
