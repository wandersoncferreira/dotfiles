;;; setup-keybindings --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:

(global-set-key (kbd "C-x p") 'pop-to-mark-command)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-m") 'smex)

(global-set-key (kbd "M-x") 'smex)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
