;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(package-initialize)

;; HOOKS
;; tabs to 2 spaces woohoo
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
;; prettier
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'markdown-mode-hook 'prettier-js-mode)

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
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(package-selected-packages
   (quote
    (magit prettier-js find-file-in-project flycheck json-mode yaml-mode markdown-mode auto-complete sass-mode pug-mode zenburn-theme solarized-theme rjsx-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; my custom stuff

;; use project prettier files
(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))

;; load default theme
(load-theme 'zenburn t)

;; auto-complete
(global-auto-complete-mode t)
(defun auto-complete-mode-maybe ()
  "no maybe"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

;; auto-pair
(electric-pair-mode 1)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "rgba(0,0,0,0.1)")
(set-face-foreground 'highlight nil)

;; line numbers
(global-linum-mode t)
(setq linum-format "%d ")
(set-face-foreground 'linum "#ccc")

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
;; file specific tab-widths
(setq scss-indent-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; paren highlight matching
(show-paren-mode 1)

;; go away bell
(setq ring-bell-function 'ignore)

;; buffer navigation
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; put file info in title bar
(setq frame-title-format
      '(buffer-file-name "%b - %f" ; File buffer
       (dired-directory dired-directory ; Dired buffer
       (revert-buffer-function "%b" ; Buffer Menu
       ("%b - Dir: " default-directory))))) ; Plain buffer

;; auto-save and backup dir
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; CUSTOM KEY BINDINGS
;; find-file-in-project
(global-set-key (kbd "C-x j") 'find-file-in-project)
;; magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; end of file

