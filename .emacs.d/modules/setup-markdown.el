;;; setup-markdown --- Markdown mode
;;
;;; Commentary:
;;
;;; Code:

;; * External Dependencies

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))


(provide 'setup-markdown)
;;; setup-markdown.el ends here
