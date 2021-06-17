;;; +extra-ledger.el -*- lexical-binding: t; -*-

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

(setq
 ledger-reports
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
