;;; ../code/dotfiles/.doom.d/+extra-editing.el -*- lexical-binding: t; -*-

(defun bk/point-to-register ()
  "Store cursor position in a register."
  (interactive)
  (point-to-register 8)
  (message "Point set"))

 (defun bk/jump-to-register ()
  "Switch between current pos and stored pos."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(defun bk/kill-inner-word ()
  "Equivalent to ciw in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(defun bk/copy-whole-line ()
  "Copies a line without refard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(defun bk/zap-to-char-backward (arg char)
  (interactive "p\ncZap up to char backward: ")
  (save-excursion
    (zap-up-to-char -1 char)))

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

(define-abbrev-table 'global-abbrev-table
  '(
    ("reuslt" "result" nil 0)
    ("requier" "require" nil 0)
    ))

(map!
 "C-c r p" #'bk/point-to-register
 "C-c r j" #'bk/jump-to-register
 "C-c k w" #'bk/kill-inner-word
 "C-c k f" #'zap-up-to-char
 "C-c k b" #'bk/zap-to-char-backward
 "C-c y l" #'bk/copy-whole-line

 ;; abbrev
 "C-x a l" #'bk/add-region-local-abbrev
 "C-x a g" #'bk/add-region-global-abbrev)
