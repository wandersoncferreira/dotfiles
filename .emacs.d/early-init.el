;;; early-init.el --- Early customizations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-pinned-packages '())

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash)
  (package-install 'diminish))

(defvar lisps-dir (expand-file-name "lisps" user-emacs-directory))

(add-to-list 'load-path lisps-dir)

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(defun bk/reinstall-package (pkg)
  "Reinstall PKG that are not properly byte-compiled."
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(defun bk/packages-count ()
  "How many packages do I have installed."
  (interactive)
  (message "Packages installed: %s" (length package-alist)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(provide 'early-init)
;;; early-init.el ends here
