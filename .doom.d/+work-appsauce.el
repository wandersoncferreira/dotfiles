;;; +work-appsauce.el -*- lexical-binding: t; -*-

(defun bk/create-worktree ()
  "Help development on multiple branches."
  (interactive)
  (let* ((root-proj (projectile-project-root))
         (proj-name (car (cdr (nreverse (split-string root-proj "/")))))
         (dest-dir (file-name-directory (directory-file-name root-proj)))
         (branch (ido-completing-read "Choose the branch: " (magit-list-local-branch-names)))
         (worktree-path (concat dest-dir proj-name "-wt-" branch)))
    (magit-worktree-checkout worktree-path branch)
    (projectile-find-file)))

(defun bk/delete-worktree ()
  "Delete worktree and all its open buffers."
  (interactive)
  (let ((worktree (ido-completing-read "Choose worktree: " (magit-list-worktrees))))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (let ((worktree-name (file-name-base worktree)))
                (when (string-equal (projectile-project-name) worktree-name)
                  (kill-buffer buffer)))))
          (buffer-list))
    (magit-worktree-delete worktree)))
