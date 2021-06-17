;;; +work-reifyhealth.el -*- lexical-binding: t; -*-

(defun reifyhealth/cider-connect ()
  "Connect into eSource."
  (interactive)
  (cider-connect-clj (list :host "localhost" :port 12344)))

(defun reifyhealth ()
  "Open file notes from work."
  (interactive)
  (find-file "~/repos/reifyhealth/work.org"))

(defun bk/uuid ()
  "Create uuid and add to clipboard."
  (interactive)
  (kill-new (uuidgen-4)))
