;;; ../dotfiles/.doom.d/+extra-macos.el -*- lexical-binding: t; -*-

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


(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier '(:ordinary super :button 2)
        alert-default-style 'osx-notifier))
