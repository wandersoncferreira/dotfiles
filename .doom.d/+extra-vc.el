;;; ../dotfiles/.doom.d/+extra-vc.el -*- lexical-binding: t; -*-

(setq magit-diff-refine-hunk t
      magit-section-initial-visibility-alist
      `((untracked . show)
        (unstaged . show)
        (unpushed . show)
        (unpulled . show)
        (stashes . show)))

(require 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-github-set-url-format)

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

(map! "s-g" #'gh-notify

      :map forge-post-mode-map
      "C-c C-d" #'bk/post-draft-pull-request

      :map diff-mode-map
      "C-c s" #'bk/github-review--copy-suggestion

      :map forge-topic-mode-map
      "C-c r" #'github-review-forge-pr-at-point)
