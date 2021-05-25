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

(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 160)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(defun rkn/org-babel-tangle-dont-ask ()
 (when (string-equal (buffer-file-name) (expand-file-name "~/.config/dotfiles/modules/editors/emacs/config/emacs.org"))
   (let ((org-confirm-babel-evaluate nil))
     (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rkn/org-babel-tangle-dont-ask
					     'run-at-end 'only-in-org-mode)))
