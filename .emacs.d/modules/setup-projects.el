;;; setup-projects --- Add shortcuts
;;
;;; Commentary:
;;
;;; Code:


;; * External Dependencies

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ido
        projectile-dynamic-mode-line nil
        projectile-ignored-projects '("~/" "/tmp")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "TAGS" "ido.last" "recentf" "smex-items")
        )
  :config
  (require 'subr-x)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; find file in project, with specific patterns
(defun ffip--create-exclude-find-options (names)
  "Exclude NAMES from find candidates."
  (mapconcat (lambda (name) (concat "-not -regex \".*" name ".*\"")) names " "))

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-find-options
        (ffip--create-exclude-find-options
         '("/node_modules/"
           "/target/"
           "/out/"
           "/.shadow-cljs/"
           "/.cpcache/"))))

;; * Functions

(defun ffip-create-pattern-file-finder (&rest patterns)
  "Create new functions that look for a specific PATTERNS."
  (let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; * Keybindings

(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o jn") (ffip-create-pattern-file-finder "*.json"))
(global-set-key (kbd "C-x C-o ht") (ffip-create-pattern-file-finder "*.html"))
(global-set-key (kbd "C-x C-o ed") (ffip-create-pattern-file-finder "*.edn"))
(global-set-key (kbd "C-x C-o ym") (ffip-create-pattern-file-finder "*.yml"))

(provide 'setup-projects)
;;; setup-projects.el ends here
