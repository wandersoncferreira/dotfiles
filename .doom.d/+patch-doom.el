;;; ../dotfiles/.doom.d/+patch-doom.el -*- lexical-binding: t; -*-

;; I want to overwrite the load! macro to take GPG files into account
(defmacro load! (filename &optional path noerror gpg?)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let* ((path (or path
                   (dir!)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                   `(expand-file-name ,filename ,path)
                 filename))
         (file (if gpg?
                   `(concat ,file ".el.gpg")
                 file)))
    `(condition-case-unless-debug e
         (if ,gpg?
             (load-file ,file)
           (let (file-name-handler-alist)
             (load ,file ,noerror 'nomessage)))
       (doom-error (signal (car e) (cdr e)))
       (error (doom--handle-load-error e ,file ,path)))))
