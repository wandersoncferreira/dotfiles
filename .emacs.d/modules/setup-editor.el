;;; setup-editor --- Editor.... we should be inside a text editor, right?
;;
;;; Commentary:
;;
;;; Code:

;; * Functions

(defun bk/kill-inner-word ()
  "Kill the entire word your cursor is in.  Equivalent to ciw in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "C-c k w") 'bk/kill-inner-word)

(defun duplicate-region (num &optional start end)
  "Duplicate the region bounded by START and END NUM times."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (_ num)
      (insert region))))

;; * External Dependencies

(use-package jump-char
  :ensure t
  :bind (("M-n" . jump-char-forward)
         ("M-p" . jump-char-backward)))

(use-package expand-region
  :ensure t
  :config
  (setq expand-region-fast-keys-enabled nil
        er--show-expansion-message t)
  :bind ("C-=" . er/expand-region))

(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package fix-word
  :ensure t
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("<mouse-3>" . mc/add-cursor-on-click)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(trailing tabs tab-mark)
        whitespace-line-column 100)
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(provide 'setup-editor)
;;; setup-editor.el ends here
