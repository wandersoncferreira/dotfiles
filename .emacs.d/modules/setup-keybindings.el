;;; setup-keybindings --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:

(global-set-key (kbd "C-x p") 'pop-to-mark-command)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-m") 'smex)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-c |") 'bk/toggle-window-split)

(global-set-key (kbd "C-x k") 'bk/kill-buffer)

(global-set-key (kbd "C-x 3") 'bk/hsplit-last-buffer)

(global-set-key (kbd "C-x 2") 'bk/vsplit-last-buffer)

(global-set-key (kbd "C-c m p") 'bk/point-to-register)

(global-set-key (kbd "C-c j p") 'bk/jump-to-register)

;; * External Dependencies

(use-package eldoc
  :diminish eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil)
  :config
  (global-eldoc-mode +1))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
