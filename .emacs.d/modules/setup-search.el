;;; setup-search --- Search Processes
;;
;;; Commentary:
;;
;;; Code:

;; * Keybindings

;; remap M-s .  because M-s was taken by paredit. Similar to vim * command
(global-set-key (kbd "C-c .") 'isearch-forward-symbol-at-point)


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
