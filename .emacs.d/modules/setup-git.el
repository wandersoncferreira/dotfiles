;;; setup-git --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:


;; * Functions

(require 'dash)

(defun util/get-frame->selected-window ()
  "Return a list of pairs of (frame `selected-window')."
  (let* ((original-frame (window-frame))
         (result (->> (visible-frame-list)
                   (-map (lambda (f)
                           (select-frame f t)
                           (list f (selected-window)))))))
    (select-frame original-frame t)
    result))

(defun util/preserve-selected-window (f)
  "Run the given function F and then restore focus to the original window.
Useful when you want to invoke a function (like showing
documentation) but desire to keep your current window focused."
  (let* ((original-frame (selected-frame))
         (frames->windows (util/get-frame->selected-window))
         (result (funcall f)))
    (-each frames->windows (lambda (x)
                             (select-frame (first x) t)
                             (select-window (second x) t)))
    (select-frame-set-input-focus original-frame t)
    result))

(defun init-git-commit-mode ()
  "Move cursor to the beginning of the buffer when the git commit window is shown."
  (end-of-line))

(defun with-magit-output-buffer (f)
  "Display the result of function F in a new buffer."
  (let ((f f))
    (util/preserve-selected-window
     (lambda ()
       (funcall f)
       (display-buffer (magit-process-buffer t))))))


(defun git-pull ()
  "Run git pull in a dedicated buffer."
  (interactive)
  (with-magit-output-buffer (lambda () (call-interactively 'magit-pull-from-upstream))))

;; * External Dependencies

(use-package magit
  :ensure t
  :init
  (setq magit-log-show-gpg-status t
        magit-completing-read-function 'magit-ido-completing-read

        ;; when committing, don't have Magit show the diff of what's changed
        magit-commit-show-diff nil

        ;; don't refresh the status buffer unless it's currently focused. This should increase performace
        magit-refresh-status-buffer nil

        ;; have magit open buffers in the current window, rather than a new split
        magit-display-buffer-function (lambda (buffer)
                                        (display-buffer buffer '(display-buffer-same-window)))
        magit-section-initial-visibility-alist
        '((untracked . show)
          (unstaged . show)
          (unpushed . show)
          (unpulled . show)
          (stashes . show)))
  :bind (("C-c g s" . magit-status)
         ("C-c g b" . magit-blame))
  :config
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes unstage-all-changes))

  (add-hook 'git-commit-mode-hook 'init-git-commit-mode)

  ;; Real dates, please
  (set-default 'magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package git-link
  :ensure t
  :config
  (setq git-link-open-in-browser t))

(use-package magit-todos
  :ensure t
  :commands (magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 100))

(use-package forge
  :ensure t
  :config
  (setq forge-topic-list-limit '(10 . -1))
  :bind
  ("C-c f r" . forge-list-requested-reviews)
  ("C-c f p" . forge-list-authored-pullreqs)
  ("C-c f c" . forge-create-pullreq))

(use-package git-timemachine :ensure t)

(use-package gitconfig-mode
  :ensure t
  :config
  (require 'gitconfig-mode))

(use-package gitignore-mode
  :ensure t
  :config
  (require 'gitignore-mode))

(use-package gitignore-templates :ensure t)

(use-package gist
  :ensure t
  :commands (gist-region-or-buffer))

(provide 'setup-git)
;;; setup-git.el ends here
