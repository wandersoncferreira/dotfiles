;;; ../dotfiles/.doom.d/+patch-counsel.el -*- lexical-binding: t; -*-

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
