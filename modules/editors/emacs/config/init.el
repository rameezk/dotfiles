(server-start)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 160)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package zenburn-theme
  :config
  (setq zenburn-use-variable-pitch t)
;; scale headings in org-mode
(setq zenburn-scale-org-headlines t)
;; scale headings in outline-mode
(setq zenburn-scale-outline-headlines t)

;; enable theme
(load-theme 'zenburn t))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(setq comp-async-report-warnings-errors nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package vertico
  :init
  (vertico-mode)

  ;; Wrap around list
  (setq vertico-cycle t)
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(defun rkn/org-babel-tangle-dont-ask ()
 (when (string-equal (buffer-file-name) (expand-file-name "~/.config/dotfiles/modules/editors/emacs/config/emacs.org"))
   (let ((org-confirm-babel-evaluate nil))
     (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rkn/org-babel-tangle-dont-ask
					     'run-at-end 'only-in-org-mode)))

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet ?\s
	org-superstar-leading-fallback ?\s
	org-hide-leading-stars nil
	org-superstar-todo-bullet-alist
	'(("TODO" . 9744)
	  ("[ ]"  . 9744)
	  ("DONE" . 9745)
	  ("[X]"  . 9745)))
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  :after (org))

(use-package org)
