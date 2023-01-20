(setq comp-async-report-warnings-errors nil)

(server-start)

(setq straight-use-package-by-default t)

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

(setq inhibit-startup-message t)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(menu-bar-mode -1)

(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 180)

;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;;(load-theme 'doom-gruvbox t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package zenburn-theme
;;   :config
;;   (setq zenburn-use-variable-pitch t)
;;   ;; scale headings in org-mode
;;   (setq zenburn-scale-org-headlines t)
;;   ;; scale headings in outline-mode
;;   (setq zenburn-scale-outline-headlines t)
  
;;   ;; enable theme
;;   (load-theme 'zenburn t))

;; (use-package poet-theme
;;   :config
;;   ;; enable theme
;;   (load-theme 'poet t))

(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-winter :no-confirm))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package page-break-lines)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil))
  (fset 'evil-visual-update-x-selection 'ignore))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
    :global-prefix "M-SPC"
    :major-modes t))

(setq ispell-program-name "aspell")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode))

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

(use-package consult
  :init
  (setq consult-project-root-function #'projectile-project-root)

  :config
  (setq consult-find-args (concat "find . "
				  "-not ( -wholename */.git/* -prune ) "
				  "-not ( -wholename */.venv/* -prune ) ")))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package company
  :config
  (global-company-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package magit)

(use-package pinentry
  :config
  
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package org)

(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-todo-keywords
      '((sequence "TODO" "BLOCKED" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("BLOCKED" . "red")))

(setq current-journal-file (expand-file-name (format-time-string "~/Dropbox/DigitalGarden/journals/%Y-%m-%b.org")))

(setq org-capture-templates
      '(("p" "Day Planning" entry (file+olp+datetree current-journal-file)
"* Day Planning %U
** Thoughts / diary / fleeting notes
** Tasks for today [/]
*** TODO %?
** Tasks that will satisfy end-of-the-day me [/]
** Focus Blocks
** Habits [/]
- [ ] Are you satisfied with the number of pomodori?
- [ ] Did you tend to your Digital Garden?")))

(use-package org-superstar
  :after (org)
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
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-autolist
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

(use-package org-roam
  :after (org)
  :hook 
  (after-init . org-roam-mode)
  (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/Dropbox/DigitalGarden")
  :config
  (setq org-roam-graph-exclude-matcher '("inbox"))
  (setq org-roam-node-display-template "${title} ${tags}"))

(defun rkn/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

(setq org-roam-dailies-capture-templates
      '(("d"
         "daily"
         plain
         (function org-roam-capture--get-point)
         "** %<%H:%M> %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+TITLE: Daily - %<%A %Y-%m-%d>\n\n* %<%A> %<%Y-%m-%d>")))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq org-agenda-files '("~/Dropbox/DigitalGarden/journals/"))

(setq org-startup-folded t)

(add-hook 'org-mode-hook 'flyspell-mode)

(setq bookmark-fontify nil)

(use-package olivetti)

(setq org-return-follows-link t)

(setq org-startup-indented t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(defun rkn/org-babel-tangle-dont-ask ()
  (when (string-equal (buffer-file-name) (expand-file-name "~/.config/dotfiles/modules/editors/emacs/config/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rkn/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

(use-package org-pomodoro)

(setq org-agenda-custom-commands
      '(
	("c" . "My Custom Agendas")

	("cu" "Unscheduled TODO"
	 ((todo ""
		((org-agenda-overriding-header "\nUnscheduled TODO")
		 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
	 nil
	 nil)

	("cw" "Weekly Review"
	 ((agenda ""
		  ((org-agenda-start-day "-7d")
		   (org-agenda-span 14)
		   (org-agenda-start-on-weekday 1)
		   (org-agenda-archives-mode t)))
	  (alltodo "")))

	("n" "Agenda and all TODOs"
	 ((agenda #1="")
	  (alltodo #1#)))))

(use-package clojure-mode
  :after (flycheck-clj-kondo)
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'lsp)
  (add-hook 'clojurescript-mode-hook 'enable-paredit-mode)
  (add-hook 'clojurescript-mode-hook 'lsp)
  (add-hook 'clojurec-mode-hook 'lsp)

  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)

  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-minimum-prefix-length 1
        lsp-lens-enable t
        lsp-signature-auto-activate nil
                                        ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
                                        ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
        ))

(use-package flycheck-clj-kondo)

(use-package cider)

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojurescript-mode-hook 'clj-refactor-mode)
  :diminish clj-refactor-mode)

(use-package aggressive-indent
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojurescript-mode-hook #'aggressive-indent-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook 'rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(use-package lispyville
  :after (org)
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook clojure-mode-hook clojurescript-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional commentary slurp/barf-cp)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'gfm-mode-hook 'visual-line-mode)

(add-hook 'markdown-mode-hook 'flyspell-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(add-to-list 'auto-mode-alist '("\\Pipfile\\'" . conf-toml-mode))

(use-package yaml-mode
  :mode "\\.(yml|yaml)\\'")

(use-package rustic)

(use-package terraform-mode)

(use-package nim-mode
  :ensure t
  :hook
  (nim-mode . lsp))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-nixfmt-bin "~/.nix-profile/bin/nixfmt"))

(defun rkn/reload-emacs-config()
  (interactive)
  (load-file user-init-file))

(rkn/keymap-define-global
  ;; grep current file quickly
  "/" 'consult-line
  
  "f" '(:ignore t :which-key "file")
  "f f" 'find-file
  "f d" '(:ignore t :which-key "dot")
  "f d e" '((lambda() (interactive)(find-file "~/.config/dotfiles/modules/editors/emacs/config/emacs.org")) :which-key "dot-edit")
  "f d i" '((lambda() (interactive)(find-file user-init-file)) :which-key "dot-edit")
  "f d r" '((lambda() (interactive)(rkn/reload-emacs-config)) :which-key "reload-emacs-config")
  "f d R" '((lambda() (interactive)(shell-command "dot rebuild")(rkn/reload-emacs-config)) :which-key "reload-dotfiles"))

(rkn/keymap-define-global
  ;; grep current file quickly
  "g" '(:ignore t :which-key "git")
  "g g" 'magit-status
  "g b" 'magit-blame)

(rkn/keymap-define-global
  ;; grep current file quickly
  "p" '(:ignore t :which-key "project")
  "p p" 'projectile-switch-project
  "p s" 'consult-ripgrep
  "SPC" 'consult-find)

(rkn/keymap-define-global
  "b" '(:ignore t :which-key "buffer")
  "bb" 'consult-buffer)

(rkn/keymap-define-global
  "n" '(:ignore t :which-key "note")
  "nr" '(:ignore t :which-key "roam")
  "nrf" 'org-roam-node-find
  "nri" 'org-roam-node-insert
  "nrc" '((lambda()
	    (setq current-journal-file
		  (expand-file-name
		   (format-time-string "~/Dropbox/DigitalGarden/journals/%Y-%m-%b.org")))
	    (interactive)(org-capture)) :which-key "org-capture")
  "nrd" '((lambda() (interactive)(find-file (format-time-string "~/Dropbox/DigitalGarden/journals/%Y-%m-%b.org"))) :which-key "Daily Journal")
  "nrs" 'rkn/org-roam-rg-search
  "nrb" 'org-roam-buffer-toggle)

(general-define-key
 "C-SPC" 'company-complete)

(rkn/keymap-define-global
  "a" 'org-agenda)

(rkn/keymap-define-global
  "t" '(:ignore t :which-key "toggle")
  "tt" 'ef-themes-toggle
  "tf" 'toggle-frame-fullscreen
  "tl" 'linum-mode)

(rkn/keymap-define-map
  :keymaps 'org-mode-map
  "m" '(:ignore t :which-key "org")
  "m SPC" 'consult-outline
  "m c" '(:ignore t :which-key "clock")
  "m c i" 'org-clock-in
  "m c o" 'org-clock-out
  "m e" '(:ignore t :which-key "edit")
  "m e s" 'org-edit-src-code)

(rkn/keymap-define-map
  :keymaps 'clojure-mode-map 
  "m" '(:ignore t :which-key "clojure")
  ;; cider
  "m c" '(:ignore t :which-key "cider")
  "m c c" 'cider-connect-clj
  "m c s" 'cider-connect-cljs
  "m e" '(:ignore t :which-key "eval")
  "m e e" 'cider-eval-last-sexp
  "m e c" 'cider-eval-defun-to-comment
  "m r" '(:ignore t :which-key "repl")
  "m r n" 'cider-repl-set-ns)

(rkn/keymap-define-map
  :keymaps 'nix-mode-map
  "m" '(:ignore t :which-key "nix")
  "m f" 'nix-format-buffer)
