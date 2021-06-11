;;; setup-finance --- Ledger
;;
;;; Commentary:
;;
;;; Code:


;; * Functions

(defun bk/clean-leader-on-save ()
  "Hook to keep the file organized."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

(defun bk/encoded-date (date)
  "Create DATE."
  (string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" date)
  (let* ((fixed-date
          (concat (match-string 1 date) "-"
                  (match-string 2 date) "-"
                  (match-string 3 date)))
         (d (parse-time-string fixed-date)))
    (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun bk/ledger-change-date (num)
  "Change the date based on NUM of days."
  (save-excursion
    (ledger-navigate-beginning-of-xact)
    (let* ((beg (point))
           (end (re-search-forward ledger-iso-date-regexp))
           (xact-date (filter-buffer-substring beg end)))
      (delete-region beg end)
      (insert
       (format-time-string
        "%Y/%m/%d"
        (time-add (bk/encoded-date xact-date)
                  (days-to-time num)))))))

(defun bk/ledger-increment-date ()
  "Increase the date number."
  (interactive)
  (bk/ledger-change-date 1))


(defun bk/ledger-decrement-date ()
  "Decrease the date number."
  (interactive)
  (bk/ledger-change-date -1))

(defun bk/copy-ledger-entry ()
  "Copy last ledger entry."
  (interactive)
  (save-excursion
    (backward-sentence)
    (let ((beg (point)))
      (forward-sentence)
      (kill-ring-save beg (point))))
  (yank))

(defun bk/clean-ledger ()
  "Bring back timeline structure to the whole file."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

;; * External Dependencies

(use-package ledger-mode
  :ensure t
  :mode ("ledger" . ledger-mode)
  :init
  :custom
  (ledger-reports
   '(("netcash" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -R -X R$ --current bal ^assets:bank ^assets:crypto liabilities:card")
     ("sports" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:sports")
     ("doctor" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:doctor")
     ("apartamento-mae" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ -S date --current -w reg ^liabilities:apartment:mother")
     ("apartamento-misce" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ -S date --current -w reg ^liabilities:apartment:misce")
     ("eas-profit" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --invert --current bal ^expenses:eval ^income:eval")
     ("food" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:food")
     ("donation" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:donation")
     ("apartamento-morumbi" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:house")
     ("creta" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^expenses:car:creta ^equity:car:creta")
     ("networth" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^assets:bank liabilities equity:apartment")
     ("spent-vs-earned" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger bal -X BRL --period=\"last 4 weeks\" ^Expenses ^Income --invert -S amount")
     ("budget" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -X R$ --current bal ^assets:bank:checking:budget liabilities:card")
     ("taxes" "ledger [[ledger-mode-flags]] -f /home/wanderson/ledger -R -X R$ --current bal ^expenses:taxes")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :config
  (setq ledger-report-auto-width t)
  (add-hook 'before-save-hook 'bk/clean-leader-on-save)
  :bind (:map ledger-mode-map
              ("C-M-." . bk/ledger-increment-date)
              ("C-M-," . bk/ledger-decrement-date)))

(use-package hledger-mode
  :ensure t
  :bind (:map hledger-mode-map
              ("TAB" . completion-at-point)))

(provide 'setup-finance)
;;; setup-finance.el ends here
