;;; ../dotfiles/.doom.d/+extra-search.el -*- lexical-binding: t; -*-

(require 'deadgrep)

(defun deadgrep--include-args (rg-args)
  (push "--hidden" rg-args) ;; consider hidden folders/files
  (push "--follow" rg-args) ;; follow symlink
  )

(advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args)
