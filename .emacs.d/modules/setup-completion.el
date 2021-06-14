;;; setup-completion --- Changing Emacs completion
;;
;;; Commentary:
;;
;; Very useful write up about IDO can be found here: https://www.masteringemacs.org/article/introduction-to-ido-mode
;;
;;
;;; Code:

;; * Mode Activation

(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; * Attributes

;; match strings partially
(setq ido-enable-flex-matching t)

;; stop asking if should create new buffers
(setq ido-create-new-buffer 'always)

;; order in which IDO displays the files
(setq ido-file-extensions-order '(".clj" ".sql" ".md" ".org"))

(setq confirm-nonexistent-file-or-buffer nil)

(setq ido-auto-merge-work-directories-length -1)

(setq ido-confirm-unique-completion t)

;; make usage of `completion-ignored-extensions' in ido
(setq ido-ignore-extensions t)

;; files and directories to be ignored by IDO
(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories ".cpcache")
(add-to-list 'ido-ignore-directories "eln-cache")

(require 'patch-hippie-expand)

(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-x l") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;; * Keybindings

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

(use-package ido
  :bind (:map ido-file-completion-map
              ("C-n" . ido-next-match)
              ("C-p" . ido-prev-match)
              ("C-w" . ido-delete-backward-updir)
              ("C-x C-w" . ido-copy-current-file-name)
              :map ido-common-completion-map
              ("SPC" . self-insert-command)
              ("M-SPC" . just-one-space)
              :map ido-file-dir-completion-map
              ("C-w" . ido-delete-backward-updir)
              ("C-x C-w" . ido-copy-current-file-name)))

;; * External Dependencies

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode +1))

(use-package ido-at-point
  :ensure t
  :after ido
  :config
  (ido-at-point-mode +1))

(use-package ido-completing-read+
  :ensure t
  :after ido
  :config
  (ido-ubiquitous-mode +1))

(use-package smex :ensure t)

(provide 'setup-completion)
;;; setup-completion.el ends here
