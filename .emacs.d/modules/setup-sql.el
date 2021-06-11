;;; setup-sql --- SQL programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies:

;; reformat SQL using external program pgformatter
(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter)
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))


(use-package sql-indent
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(provide 'setup-sql)
;;; setup-sql.el ends here
