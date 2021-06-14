;;; setup-sql --- SQL programming language
;;
;;; Commentary:
;;
;;; Code:

(add-hook 'sql-mode-hook 'electric-pair-mode)

;; * External Dependencies:

;; reformat SQL using external program pgformatter
(use-package sqlformat
  :ensure t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1"))
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))


(use-package sql-indent
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(provide 'setup-sql)
;;; setup-sql.el ends here
