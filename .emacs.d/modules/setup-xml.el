;;; setup-xml --- XML programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package nxml-mode
  :init
  (setq nxml-child-indent 4)
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode)))


(provide 'setup-xml)
;;; setup-xml.el ends here
