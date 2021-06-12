(defun bk/align-whitespace (start end)
  "Align columns by whitespace from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun bk/align-ampersand (start end)
  "Align columns by ampersand from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun bk/align-quote-space (start end)
  "Align columns by quote and space from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))

(defun bk/align-equals (start end)
  "Align columns by equals sign from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))

(defun bk/align-comma (start end)
  "Align columns by comma from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))

(defun bk/align-dot (start end)
  "Align columns by dot from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))

(defun bk/align-colon (start end)
  "Align columns by equals sign from START to END."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))

(provide 'align-defun)
;;; align-defun.el ends here
