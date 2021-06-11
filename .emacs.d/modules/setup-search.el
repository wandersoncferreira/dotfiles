;;; setup-search --- Search Processes
;;
;;; Commentary:
;;
;;; Code:

(use-package imenu
  :config
  (setq imenu-auto-rescan 1
        imenu-auto-rescan-maxout 600000
        imenu-max-item-length 600
        imenu-use-markers t
        imenu-max-items 200)
  :bind
  ("C-c i" . imenu))

;;  * Functions

(defun bk/occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; * Keybindings

;; remap M-s .  because M-s was taken by paredit. Similar to vim * command
(global-set-key (kbd "C-c .") 'isearch-forward-symbol-at-point)

(global-set-key (kbd "C-c o") 'bk/occur-dwim)


;; * External Dependencies

(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t
        ag-reuse-window t))

(use-package rg
  :ensure t
  :commands (bk/search-git-root-or-dir)
  :config
  (rg-define-search bk/search-git-root-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc vc default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package wgrep :ensure t)


(provide 'setup-search)
;;; setup-search.el ends here
