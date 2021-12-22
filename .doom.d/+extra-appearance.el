;;; ../dotfiles/.doom.d/+extra-appearance.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-zenburn
      doom-font (font-spec :family "Monaco" :size 14)
      doom-themes-treemacs-theme "all-the-icons"
      fill-column 180
      display-line-numbers-type nil
      confirm-kill-emacs nil
      indent-tabs-mode nil)

(when IS-LINUX
  (setq doom-font (font-spec :family "Source Code Pro" :size 15))
  (setq doom-theme 'modus-operandi))

;; delete selection
(delete-selection-mode +1)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode -1)))

(defun bk/default-theme ()
  "Change highlight colors when using the default white theme."
  (set-face-attribute 'lazy-highlight nil :background "khaki1")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

(defun bk/slate-gray-theme ()
  "Slate Grat"
  (interactive)
  (set-background-color "DarkSlateGray")
  (set-face-background 'mode-line "Wheat")
  (set-face-foreground 'mode-line "DarkSlateGray")
  (set-foreground-color "Wheat"))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; hacky: avoid saving custom faces in custom.el files
(defun custom-save-faces ()
  "No-op"
  (interactive))

(defun bk/alect-themes-customizations ()
  (custom-set-faces
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(font-lock-variable-name-face ((t (:foreground "#6a621b"))))
   '(ivy-current-match ((t (:extend t :background "#b0d0f3" :foreground "#101010" :weight bold))))
   '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "#2020cc" :underline (:color "dark orange" :style wave)))) t)))

(defun bk/high-contrast-customizations ()
  (custom-set-faces
   '(mode-line ((t (:background "Gray75" :foreground "Black"))))
   '(mode-line-buffer-id ((t (:background "Gray75" :foreground "blue4"))))
   '(mode-line-mousable ((t (:background "Gray75" :foreground "firebrick"))))
   '(mode-line-mousable-minor-mode ((t (:background "Gray75" :foreground "green4"))))))

(defun bk/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         80
       100))))

(defun bk/default-black-customizations ()
  "Customizations to be used with default black theme in Doom Emacs"
  (custom-set-faces
   '(doom-modeline-buffer-path ((t (:foreground "black" :weight bold))))
   '(doom-modeline-project-dir ((t (:foreground "black" :weight bold))))
   '(doom-modeline-buffer-modified ((t (:foreground "Blue" :weight bold))))
   '(success ((t (:foreground "ForestGreen" :weight bold))))))

;; control the modeline info
(use-package! delight
  :config
  (delight '(;; general
             (whitespace-mode nil whitespace)
             (vi-tilde-fringe-mode nil vi-tilde-fringe)
             (ws-butler-mode nil ws-butler)
             (eldoc-mode nil eldoc)
             (gcmh-mode nil gcmh)
             (which-key-mode nil which-key)
             (better-jumper-local-mode nil better-jumper)
             (company-mode nil company)
             (company-box-mode nil company-box)
             (paredit-mode nil paredit)
             (ivy-mode nil ivy)
             (ivy-posframe-mode nil ivy-posframe)
             (org-roam-mode nil org-roam)
             (git-gutter-mode nil git-gutter)
             (volatile-highlights-mode nil volatile-highlights)
             (yas-minor-mode nil yasnippet)
             (abbrev-mode nil abbrev)
             (annotate-mode nil annotate)
             (symbol-focus-mode nil symbol-focus)
             (lsp-lens-mode nil lsp-lens)

             ;; clojure
             (clj-refactor-mode nil clj-refactor)
             (dtrt-indent-mode nil dtrt-indent))))

(use-package! projectile
  :delight projectile-mode)
