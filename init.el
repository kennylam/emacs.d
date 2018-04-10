;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; thsi is the one that sets tabs to 2 spaces woohoo
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(package-initialize)


;;; Fonts

;; We use these fonts:
;;
;; - Monoid (http://larsenwork.com/monoid/) as default
;; - XITS Math (https://github.com/khaledhosny/xits-math) as fallback for math
;;
;; Source Code Pro (https://github.com/adobe-fonts/source-code-pro) is a good
;; monospace font, too.  An alternative is Consolas.  Another great monospace
;; font is and Pragmata Pro (http://www.fsd.it/fonts/pragmatapro.htm,
;; proprietary, around 200$).
;;
;; Currently this setup only works for OS X, as we rely on Apple's Emoji and
;; Symbol fonts.
;;
;; TODO:  Find Emoji and symbol fonts for Linux and Windows
;; TODO: need a better fallback option if these custom fonts are not available

(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 110)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 120 :weight 'regular)

;; Font setup
(defun lunaryorn-configure-fonts (frame)
  "Set up fonts for FRAME.
Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t nil (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(package-selected-packages
   (quote
    (sass-mode pug-mode zenburn-theme solarized-theme rjsx-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; my custom stuff
;; load default theme
(load-theme 'zenburn t)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; always add a trailing newline - POSIX
(setq require-final-newline t)

;; I got sick of typing "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Default indentation
(setq-default
 ;; no tabs
 indent-tabs-mode nil
 ;; 2 spaces
 tab-width 2)
(setq column-number-mode t)

;; fix indentation in rjsx-mode
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))
