;;; +extra-window.el -*- lexical-binding: t; -*-

;; shift arrow to move between buffers
(windmove-default-keybindings)

(defun bk/vsplit-last-buffer ()
  "Split the window vertically and display the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun bk/hsplit-last-buffer ()
  "Split the window horizontally and display the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

;; workaround this bug https://github.com/abo-abo/swiper/pull/2687
(defun zz/counsel-buffer-or-recentf-candidates ()
  "Return candidates for `counsel-buffer-or-recentf'."
  (require 'recentf)
  (recentf-mode)
  (let ((buffers
         (delq nil
               (mapcar (lambda (b)
                         (when (buffer-file-name b)
                           (abbreviate-file-name (buffer-file-name b))))
                       (delq (current-buffer)
                             (buffer-list))))))
    (append
     buffers
     (cl-remove-if (lambda (f)
                     (member f buffers))
                   (counsel-recentf-candidates)))))

(advice-add #'counsel-buffer-or-recentf-candidates
            :override #'zz/counsel-buffer-or-recentf-candidates)

;; list of buffers produced by `counsel-buffer-or-recentf'
;; is shown in the order the buffers have been visited.
(use-package! switch-buffer-functions
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))
