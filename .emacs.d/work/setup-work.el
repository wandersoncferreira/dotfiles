;;; setup-work --- General code used at workplaces
;;
;;; Commentary:
;;
;;; Code:

(require 'org)

;; * Functions

(defun work-new-day ()
  "Create entry into org file for bookkeeping."
  (interactive)
  (goto-char (point-max))
  (org-insert-heading-respect-content)
  (org-metaright)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
  (org-metaleft)
  (reindent-then-newline-and-indent)
  (reindent-then-newline-and-indent)
  (org-cycle)
  (insert "- "))

(provide 'setup-work)
;;; setup-work.el ends here
