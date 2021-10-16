;;; ../dotfiles/.doom.d/+patch-ivy-posframe.el -*- lexical-binding: t; -*-

(defun ivy-display-function-fallback (str)
  (let ((buffer-undo-list t))
    (save-excursion
      (forward-line 1)
      (insert str))))

(use-package! ivy-posframe
  :delight
  :after ivy-mode
  :hook (ivy-mode . ivy-posframe-mode)
  :init
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 10
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height))
        ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  :config

  ;; posframe doesn't work well with async sources (the posframe will
  ;; occasionally stop responding/redrawing), and causes violent resizing of the
  ;; posframe.
  (dolist (fn '(swiper counsel-rg counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback))

  (add-hook 'doom-after-reload-hook #'posframe-delete-all))
