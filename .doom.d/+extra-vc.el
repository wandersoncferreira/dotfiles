;;; ../dotfiles/.doom.d/+extra-vc.el -*- lexical-binding: t; -*-

(use-package! magit
  :init
  (setq magit-diff-refine-hunk t
        magit-log-show-gpg-status t
        magit-commit-show-diff nil
        magit-display-buffer-function (lambda (buf) (display-buffer buf '(display-buffer-same-window)))
        magit-section-initial-visibility-alist
        `((untracked . show)
          (unstaged . show)
          (unpushed . show)
          (unpulled . show)
          (stashes . show))))

(use-package! bug-reference-github
  :config
  (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format))

(defun bk/forge--add-draft (alist)
  "Add draft to ALIST."
  (append alist '((draft . "t"))))

(defun bk/post-draft-pull-request ()
  "Submit the post that is being edit in the current buffer as a draft."
  (interactive)
  (advice-add 'forge--topic-parse-buffer
              :filter-return #'bk/forge--add-draft)
  (condition-case err
      (forge-post-submit)
    (t
     (advice-remove 'forge--topic-parse-buffer #'bk/forge--add-draft)
     (signal (car err) (cdr err))))
  (advice-remove 'forge--topic-parse-buffer #'bk/forge--add-draft))

(use-package gh-notify
  :load-path "~/.doom.d/sources/gh-notify"
  :config
  (setq gh-notify-redraw-on-visit t))

(use-package github-review
  :load-path "~/.doom.d/sources/github-review"
  :config
  (setq github-review-view-comments-in-code-lines t
        github-review-view-comments-in-code-lines-outdated t
        github-review-reply-inline-comments t))
