;; [[file:../code/dotfiles/.doom.d/config.org::+begin_src emacs-lisp][No heading:1]]
;; -*- lexical-binding: t -*-
;; No heading:1 ends here

;; [[file:../code/dotfiles/.doom.d/config.org::*Appearance][Appearance:1]]
(setq doom-theme 'doom-zenburn)
;; Appearance:1 ends here

;; [[file:../code/dotfiles/.doom.d/config.org::*MacOS specifics][MacOS specifics:1]]
(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier '(:ordinary super :button 2)))
;; MacOS specifics:1 ends here

;; [[file:../code/dotfiles/.doom.d/config.org::*Search in Homebrew][Search in Homebrew:1]]
(defun bk/brew-search ()
  "Search homebrew for a file to be installed."
  (interactive "")
  (let* ((query (read-string "Search in Homebrew: "))
         (res (shell-command-to-string
               (format "brew search %s" query)))
         (res-list (-> res
                       (split-string "==> Formulae")
                       (-second-item)
                       (string-trim)
                       (split-string "\n")))
         (to-be-installed
          (completing-read
           "Install one of the packages: "
           res-list)))
    (when (not (string-empty-p to-be-installed))
      (async-shell-command
       (format "brew install %s" to-be-installed)))))
;; Search in Homebrew:1 ends here
