;; enable general for evil
(general-evil-setup t)

;; global keybindings
(general-define-key
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "M-SPC"

  ;; org-roam
  "n" '(:ignore t :which-key "note")
  "nr" '(:ignore t :which-key "roam")
  "nri" 'org-roam-insert
  "nrd" 'org-roam-dailies-capture-today
  "nrD" 'org-roam-dailies-find-today
  "nrf" 'org-roam-find-file)

;; nix mode keybindings
(general-define-key
  :states 'normal
  :prefix "SPC"
  :keymaps 'nix-mode-map 

  "m" '(:ignore t :which-key "nix")
  "mf" 'nix-format-buffer)
