;;; fold-clojure-setup --- provide dwim folding for Clojure buffers

;;; Commentary:

;; Based on this great work from Alpaphapha
;; https://gist.github.com/alphapapa/79ea7c33d03c9c975634559b1a776418

;;; Code:

(defcustom bk/clj-outline-regexp "\\(;;;?\\{1,8\\} \\|\\((\\w+\\)\\)"
  "Regexp to delimiter outlines in Clojure buffers."
  :group 'folding)

(defun bk/clj-outline-level ()
  (or
   (and (string-match (rx
		       (* space)
		       (group (one-or-more (syntax comment-start)))
		       (one-or-more space))
		      (match-string 0))
	(- (match-end 0) (match-beginning 0) 1))
   (+ 1 (- (match-end 0) (match-beginning 0)))))

(defun bk/fold-hide-all ()
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Skip the prelude, if any.
    (unless (outline-on-heading-p t) (outline-next-heading))
    (hide-subtree)
    (condition-case err
	(while (outline-next-heading)
	  (hide-subtree))
      (error nil))))

(defun bk/toggle-fold-subtree ()
  "Show or hide the current subtree."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
	(outline-hide-subtree)
      (outline-show-subtree))))

(defun bk/fold-show-all ()
  "For consistency reasons."
  (interactive)
  (outline-show-all))

(defun bk/toggle-fold-all ()
  "Show or hide all nodes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (outline-on-heading-p t) (outline-next-heading))
    (if (not (outline-invisible-p (line-end-position)))
	(bk/fold-hide-all)
      (bk/fold-show-all))))

(defun bk/fold-clojure-hook ()
  "Change the keybindings to your liking."
  (interactive)
  (outline-minor-mode)
  (setq outline-blank-line t
	outline-level 'bk/clj-outline-level
	outline-regexp bk/clj-outline-regexp)
  (define-key clojure-mode-map (kbd "<f5>") #'outline-previous-visible-heading)
  (define-key clojure-mode-map (kbd "<f6>") #'outline-next-visible-heading)
  (define-key clojure-mode-map (kbd "<f7>") #'bk/toggle-fold-all)
  (define-key clojure-mode-map (kbd "<f8>") #'bk/toggle-fold-subtree))

(add-hook 'clojure-mode-hook #'bk/fold-clojure-hook)

(provide 'fold-clojure)
;;; fold-clojure.el ends here
