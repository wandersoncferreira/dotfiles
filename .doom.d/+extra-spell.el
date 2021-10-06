;;; ../dotfiles/.doom.d/+extra-spell.el -*- lexical-binding: t; -*-

(defun bk/dict-pt ()
  "Change to pt-BR dictionary."
  (interactive)
  (ispell-change-dictionary "pt_BR"))

(defun bk/dict-en ()
  "Change to en dictionary."
  (ispell-change-dictionary "en"))
