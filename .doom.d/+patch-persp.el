;;; ../dotfiles/.doom.d/+patch-persp.el -*- lexical-binding: t; -*-

;;; Defines how the persp-mode show itself in the modeline.
(after! persp
  (setq persp-lighter
        '(:eval
          (format
           (propertize
            " #%.10s"
            'face (let ((persp (get-current-persp)))
                    (if persp
                        (if (persp-contain-buffer-p (current-buffer) persp)
                            'persp-face-lighter-default
                          'persp-face-lighter-buffer-not-in-persp)
                      'persp-face-lighter-nil-persp)))
           (safe-persp-name (get-current-persp))))))
