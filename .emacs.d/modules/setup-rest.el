;;; setup-rest --- REST programming language
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))

;; * Functions

(defun bk/restclient ()
  "Open a restclient buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))

(provide 'setup-rest)
;;; setup-rest.el ends here
