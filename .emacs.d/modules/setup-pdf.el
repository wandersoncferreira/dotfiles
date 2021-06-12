;;; setup-pdf --- PDF programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-continuous-scroll-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
  (require 'pdf-occur))

(provide 'setup-pdf)
;;; setup-pdf.el ends here
