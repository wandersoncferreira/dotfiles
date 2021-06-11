;;; setup-zettelkasten --- Notes
;;
;;; Commentary:
;;
;;; Code:

(defvar bk-zettelkasten-dir "/home/wanderson/zettelkasten")

;; * Functions

(defun bk/roam-add-resource ()
  "Add resource to Org Roam buffer."
  (interactive)
  (let* ((candidates (mapcar (lambda (x)
                               (replace-regexp-in-string "~/resources/" "" x))
                             (directory-files-recursively "~/resources" "")))
         (resource (ido-completing-read "Resource: " candidates))
         (link-location (concat "file:~/resources/" resource))
         (description (ido-completing-read "Description: " "")))
    (org-insert-link t link-location description)))

;; * External Dependencies

(use-package org-roam
  :ensure t
  :diminish org-roam-mode
  :init
  (setq org-roam-directory bk-zettelkasten-dir
        org-roam-completion-everywhere t
        org-roam-completion-system 'ido)
  :bind (("C-c z f" . org-roam-find-file)
         ("C-c z i" . org-roam-insert)
         ("C-c z I" . org-roam-insert-immediate))
  :config
  (org-roam-mode +1)
  (add-hook 'org-roam-backlinks-mode-hook 'toggle-truncate-lines))

(use-package org-roam-server
  :ensure t
  :commands (org-roam-server-mode)
  :init
  (setq org-roam-server-port 17042)
  :config
  (require 'org-protocol)
  (require 'org-roam-protocol))

(provide 'setup-zettelkasten)
;;; setup-zettelkasten.el ends here
