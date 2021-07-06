;;; ../dotfiles/.doom.d/themes/billc-theme.el -*- lexical-binding: t; -*-

(deftheme billc
  "Updated to Emacs 28 but original code from Bill Clementson's blog.
http://web.archive.org/web/20110927085339/http://bc.tech.coop/blog/docs/color-theme-mods.el")

(custom-theme-set-faces
 'billc
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(border ((t (:background "Blue"))))
 '(cursor ((t (:background "black"))))
 '(highlight ((t (:foreground "black" :background "darkseagreen2"))))
 '(lazy-highlight-face ((t (:foreground "dark blue" :bold t))))
 '(underline ((t (:underline t))))
 '(bold ((t (:bold t))))
 '(italic ((t (:italic t))))
 '(bold-italic ((t (:bold t :italic t))))
 '(region ((t (:background "snow3" :foreground "black"))))
 '(secondary-selection ((t (:background "paleturquoise"))))
 '(font-lock-comment-face ((t (:foreground "dark green" :italic t))))
 '(font-lock-string-face ((t (:foreground "SlateGray4" :bold t))))
 '(font-lock-doc-face ((t (:foreground ""))))
 '(font-lock-keyword-face ((t (:foreground "black" :bold t))))
 '(font-lock-builtin-face ((t (:foreground "black" :bold t))))
 '(font-lock-function-name-face ((t (:foreground "dark blue" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(font-lock-type-face ((t (:foreground "blue"))))
 '(font-lock-constant-face ((t (:foreground "dark blue"))))
 '(font-lock-warning-face ((t (:foreground "red" :bold t))))
 '(fringe ((t (:background "white"))))
 '(show-paren-match ((nil (:background "light blue"))))
 '(show-paren-mismatch ((((class color)) (:background "purple"))))
 )

(provide-theme 'billc)
