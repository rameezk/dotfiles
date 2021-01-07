;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(if (file-exists-p "~/.proxyrc")
(setq url-proxy-services
         '(("no_proxy" . "^\\(localhost\\|10.*\\)")
                ("http" . "127.0.0.1:3128")
                     ("https" . "127.0.0.1:3128"))) nil)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) 
   (package-install 'use-package))
(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
(setq use-package-verbose t)

;; On non-Guix systems, "ensure" packages by default
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 160)

(use-package color-theme-sanityinc-tomorrow)

(if (package-installed-p 'color-theme-sanityinc-tomorrow)
    (load-theme 'sanityinc-tomorrow-eighties t)
    (load-theme 'wombat t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package all-the-icons-ivy-rich
  :after (ivy counsel counsel-projectile)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package evil
   :init
   (setq evil-want-integration t)
   (setq evil-want-keybinding nil)
   :config
   (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package general
:config
(general-evil-setup t)

(general-create-definer rkn/leader-key-def 
   :keymaps '(normal insert visual emacs)
   :prefix "SPC"
   :global-prefix "C-SPC"))

(rkn/leader-key-def
"f" '(:ignore t :which-key "file")
"ff" 'counsel-find-file
"fc" (lambda() (interactive)(counsel-find-file "~/.emacs.d/emacs-config.org"))
"f/" 'swiper)

(rkn/leader-key-def 
"s" '(:ignore t which-key "swiper")
"ss" 'swiper
"sp" 'counsel-rg)

(rkn/leader-key-def 
"b" '(:ignore t :which-key "buffer")
"bb" 'counsel-switch-buffer)

(use-package magit)

(rkn/leader-key-def 
  "g" '(:ignore t :which-key "git")
  "gg" 'magit-status)

(use-package ivy
  :diminish
  :init
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package counsel-projectile)

(rkn/leader-key-def 
"p" '(:ignore t :which-key "project")
"pf" 'counsel-projectile-find-file
"ps" 'counsel-projectile-rg
"pp" 'counsel-projectile-switch-project)

(use-package ivy-rich
  :after (all-the-icons-ivy-rich)
  :init (ivy-rich-mode 1))

(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/DigitalGarden")
  :config
  (setq org-roam-graph-exclude-matcher '("private" "dailies" "Inbox" "todoist")))

(rkn/leader-key-def
"n" '(:ignore t :which-key "note")
"nr" '(:ignore t :which-key "roam")
"nrf" 'org-roam-find-file)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun rkn/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rkn/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))
