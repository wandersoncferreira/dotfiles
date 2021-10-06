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

(defun bk/github-review--copy-suggestion ()
  "Kill a region of diff+ as a review suggestion template."
  (interactive)
  (setq deactivate-mark t)
  (let ((s-region
         (buffer-substring-no-properties
          (region-beginning)
          (region-end))))
    (kill-new
     (format "# ```suggestion\n%s\n# ```\n"
             (replace-regexp-in-string "^\\+" "# " s-region)))))

(defun github-review--after-save-diff (pr-alist _diff)
  (let-alist pr-alist
    (with-current-buffer
        (format "%s___%s___%s___%s.diff" .owner .repo .num .sha)
      (goto-char (point-min)))))

(setq github-review-fetch-top-level-and-review-comments t)

(after! github-review
  (advice-add 'github-review-save-diff :after 'github-review--after-save-diff))

(use-package gh-notify
  :load-path "~/.doom.d/sources/gh-notify"
  :config
  (setq gh-notify-redraw-on-visit t))

(use-package github-review
  :load-path "~/.doom.d/sources/github-review"
  :config
  (setq github-review-view-comments-in-code-lines t
        github-review-reply-inline-comments t))

(map! :map forge-post-mode-map
      "C-c C-d" #'bk/post-draft-pull-request

      :map diff-mode-map
      "M-DEL" nil
      "C-c s" #'bk/github-review--copy-suggestion

      :map forge-topic-mode-map
      "C-x r" #'github-review-forge-pr-at-point)
