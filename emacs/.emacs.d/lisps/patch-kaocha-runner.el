;;; Kaocha --- Custom code to overwrite kaocha's implementation with my own patch
;;; Commentary:

;; I like would like to open Kaocha Output in a different frame, so I fit it better on my two screens

;;; Code:

(defvar kaocha-runner--out-buffer "*kaocha-output*")
(defvar kaocha-runner--fail-re "\\(FAIL\\|ERROR\\)")

(defmacro kaocha-runner--with-frame (buffer original-buffer &rest body)
  "Open a dedicated frame showing BUFFER, perform BODY, then switch back to ORIGINAL-BUFFER."
  `(let ((window (get-buffer-window ,buffer)))
     (if window
	 (select-window window)
       (switch-to-buffer-other-frame ,buffer))
     ,@body
     (switch-to-buffer-other-frame ,original-buffer)))

(defun kaocha-runner--show-details-window (original-buffer _)
  "Show details from the test run inside a dedicated frame a part from ORIGINAL-BUFFER."
  (kaocha-runner--with-frame kaocha-runner--out-buffer original-buffer
    (visual-line-mode 1)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (re-search-forward kaocha-runner--fail-re nil t))
    (end-of-line)
    (kaocha-runner--recenter-top)))


(provide 'patch-kaocha-runner)
;;; patch-kaocha-runner.el ends here
