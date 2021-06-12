;;; setup-spell --- Spelling
;;
;;; Commentary:
;;
;;; Code:

;; * Functions

(defun bk/spell-buffer-pt-BR ()
  "Spell check in portuguese."
  (interactive)
  (ispell-change-dictionary "pt_BR")
  (flyspell-buffer))

(defun bk/spell-buffer-en ()
  "Spell check in English."
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))

;; * External Dependencies

(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar
        "~/.emacs.d/bin/languagetool-commandline.jar"))

(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (define-key flyspell-mode-map (kbd "C-.") nil))


(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (require 'flyspell-correct-ido)
  (setq flyspell-correct-interface #'flyspell-correct-ido)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;; * Abbreviations

(defun bk/add-region-local-abbrev (start end)
  "Go from START to END and add the selected text to a local abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-mode-abbrev num-words)
        (deactivate-mark))
    (message "No selected region!")))

(defun bk/add-region-global-abbrev (start end)
  "Go from START to END and add the selected text to global abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-abbrev global-abbrev-table "Global" num-words)
        (deactivate-mark))
    (message "No selected region!")))

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(add-hook 'after-init-hook
          (lambda ()
            (abbrev-mode +1)
            (diminish 'abbrev-mode)))


(provide 'setup-spell)
;;; setup-spell.el ends here
