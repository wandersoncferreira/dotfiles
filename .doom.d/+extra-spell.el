;;; ../dotfiles/.doom.d/+extra-spell.el -*- lexical-binding: t; -*-

(defun bk/dict-pt ()
  "Change to pt-BR dictionary."
  (interactive)
  (ispell-change-dictionary "pt_BR"))

(defun bk/dict-en ()
  "Change to en dictionary."
  (ispell-change-dictionary "en"))

;; downloaded from here:
;; curl -o langtool.zip https://languagetool.org/download/LanguageTool-stable.zip && unzip langtool.zip
(setq langtool-language-tool-jar "~/Downloads/LanguageTool-5.5/languagetool-commandline.jar")
