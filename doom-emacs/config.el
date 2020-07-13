;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rameez Khan"
      user-mail-address "rameezkhan.sa@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; clojure
(setq clojure-indent-style 'align-arguments)
(setq clojure-align-forms-automatically t)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;;emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(org-agenda-files '("~/Dropbox/DigitalGarden/journal/journal-202007.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 50))

(defun my-increase-opacity()
  (interactive)
  (let ((increase (+ 10 (car (frame-parameter nil 'alpha)))))
    (if (> increase 99)(setq increase 99))
    (set-frame-parameter (selected-frame) 'alpha (values increase 75)))
  )

(defun my-decrease-opacity()
  (interactive)
  (let ((decrease (- (car (frame-parameter nil 'alpha)) 10)))
    (if (< decrease 20)(setq decrease 20))
    (set-frame-parameter (selected-frame) 'alpha (values decrease 75)))
  )

;; org-mode and org-journal
(setq org-journal-dir "~/Dropbox/DigitalGarden/journal/")
(setq org-journal-file-format "journal-%Y%m.org")
(setq org-journal-file-type 'monthly)
(setq org-journal-carryover-items "")
(map! :leader :desc "Search Narrow" "n j S" #'org-journal-search)
(map! :leader :desc "Open Current Journal" "n j o" #'org-journal-open-current-journal-file)
