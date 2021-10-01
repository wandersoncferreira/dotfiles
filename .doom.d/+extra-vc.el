;;; ../dotfiles/.doom.d/+extra-vc.el -*- lexical-binding: t; -*-

(after! magit-mode
  (setq magit-log-show-gpg-status t
        magit-commit-show-diff nil
        magit-display-buffer-function (lambda (buf) (display-buffer buf '(display-buffer-same-window)))))

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

(after! forge
  (setq forge-alist
        (append forge-alist
                '(("github.com-advthreat" "api.github.com" "github.com" forge-github-repository)))))

(use-package gh-notify
  :load-path "~/.doom.d/sources/gh-notify")

(map! :map forge-post-mode-map
      "C-c C-d" #'bk/post-draft-pull-request)
