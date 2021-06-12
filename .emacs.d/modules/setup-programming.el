;;; setup-programming --- Emacs general programming mode
;;
;;; Commentary:
;;
;;; Code:

;; * Built-in

;;; use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(use-package subword
  :diminish subword-mode
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))

;; * Functions

(defun bk/kill-all-comments ()
  "Function to kill all comments in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

;; automatically indenting yanked text if in programming modes
(require 'dash)

(defvar yank-indent-modes '(prog-mode
                            sgml-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation from BEG and END, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defun yank-unindented ()
  "Unindent after yank in programming modes."
  (interactive)
  (yank 1))

(defun yas/goto-end-of-active-field ()
  "End of the field."
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  "Start of the field."
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))


;; * External Dependencies

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode +1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode +1))

(use-package toggle-test
  :ensure t
  :init
  (setq tgt-open-in-new-window nil)
  :config
  (put 'tgt-projects 'safe-local-variable #'lisp)
  (global-set-key (kbd "H-s-t") 'tgt-toggle))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-checker-error-threshold 4000))

(use-package flycheck-clj-kondo :ensure t)

(use-package flycheck-projectile :ensure t)

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode +1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-verbosity 1
        yas-wrap-around-region t)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)

  ;; jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  :bind
  (("C-x y" . yas-expand)
   ("C-c t" . yas-describe-tables)))

(use-package yasnippet-snippets :ensure t)

(use-package quickrun :ensure t)

(use-package webpaste
  :ensure t
  :config
  (setq webpaste-provider-priority '("ix.io" "dpaste.org")))

(provide 'setup-programming)
;;; setup-programming.el ends here
