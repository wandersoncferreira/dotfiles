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

(provide 'early-init)
;;; early-init.el ends here
