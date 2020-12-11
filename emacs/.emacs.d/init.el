(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 160)

(if (package-installed-p 'color-theme-sanityinc-tomorrow)
    (load-theme 'sanityinc-tomorrow-eighties t)
    (load-theme 'wombat t))

(if (file-exists-p "~/.proxyrc")
(setq url-proxy-services
         '(("no_proxy" . "^\\(localhost\\|10.*\\)")
                ("http" . "127.0.0.1:3128")
                     ("https" . "127.0.0.1:3128"))) nil)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
(package-refresh-contents))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically tangle our config file when we save it
(defun rkn/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rkn/org-babel-tangle-config)))
