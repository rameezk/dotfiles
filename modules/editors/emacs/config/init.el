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

(setq inhibit-startup-message t)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(menu-bar-mode -1)

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(setq comp-async-report-warnings-errors nil)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general
	    :config
	    (general-evil-setup t)

	    (general-create-definer rkn/keymap-define-global
	      :keymaps '(normal insert visual emacs)
	      :prefix "SPC"
	      :global-prefix "M-SPC")

(general-create-definer rkn/keymap-define-map
    :states '(normal)
    :prefix "SPC"
    :global-prefix "M-SPC"))

(rkn/keymap-define-global
  ;; grep current file quickly
  "/" 'consult-line)

(rkn/keymap-define-global
  ;; grep current file quickly
  "g" '(:ignore t :which-key "git")
  "g g" 'magit-status)

(rkn/keymap-define-global
  ;; grep current file quickly
  "p" '(:ignore t :which-key "project")
  "p p" 'projectile-switch-project
  "SPC" 'projectile-find-file)

(rkn/keymap-define-global
  "b" '(:ignore t :which-key "buffer")
  "bb" 'consult-buffer)

(rkn/keymap-define-global
  "n" '(:ignore t :which-key "note")
  "nr" '(:ignore t :which-key "roam")
  "nrf" 'org-roam-find-file
  "nri" 'org-roam-insert
  "nrd" 'org-roam-dailies-capture-today
  "nrD" 'org-roam-dailies-find-today)

(rkn/keymap-define-map
 :keymaps 'org-mode-map 
 "m" '(:ignore t :which-key "org")
 "m SPC" 'consult-outline)

(rkn/keymap-define-map
 :keymaps 'nix-mode-map 
 "m" '(:ignore t :which-key "nix")
 "m f" 'nix-format-buffer)

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

(setq ispell-program-name "aspell")

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package magit)

(setq epa-pinentry-mode 'loopback)

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-nixfmt-bin "/home/rameezk/.nix-profile/bin/nixfmt"))

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

(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/DigitalGarden")
  :config
  (setq org-roam-graph-exclude-matcher '("inbox")))

(setq org-roam-dailies-capture-templates
      '(("d"
	 "daily"
	 entry
	 (function org-roam-capture--get-point)
	 "* %<%H:%M> %?"
	 :file-name "daily/%<%Y-%m-%d>"
	 :head "#+TITLE: Daily - %<%A %Y-%m-%d>\n\n* %<%A> %<%Y-%m-%d>")))

(setq org-startup-folded t)

(add-hook 'org-mode-hook 'flyspell-mode)
