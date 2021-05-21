{ pkgs, ... }: {

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
  };

  programs.emacs.init = {
    enable = true;

    # Turn on recommended garbage collection settings
    recommendedGcSettings = true;

    earlyInit = ''
      ;; Don't blink cursor
      (blink-cursor-mode 0)

      ;; Disable the menu bar
      (push '(menu-bar-lines . 0) default-frame-alist)

      ;; Disable the tool bar
      (push '(tool-bar-lines . 0) default-frame-alist)

      ;; Disable vertical scroll bars
      (push '(vertical-scroll-bars . nil) default-frame-alist)
    '';

    postlude = ''
      ;; disable startup message
      (setq inhibit-startup-message t)

      ;; font
      (set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 160)
    '';

    usePackage = {
      evil = {
        enable = true;
        config = ''
          (evil-mode t)
        '';
      };

      general = {
        enable = true;
        config = ''
          (general-evil-setup t)

          ;; keybindings
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
        '';
      };

      org = { enable = true; };

      org-roam = {
        enable = true;
        hook = [ "(after-init . org-roam-mode)" ];
        config = ''
          (setq org-roam-directory "~/Dropbox/DigitalGarden")
          (setq org-roam-graph-exclude-matcher '("inbox"))
        '';
      };

      org-superstar = {
        enable = true;
        after = [ "org" ];
        hook = [ "(org-mode . (lambda () (org-superstar-mode 1)))" ];
      };

      which-key = {
        enable = true;
        diminish = [ "which-key-mode" ];
        config = ''
          (setq which-key-idle-delay 0.3)
          (which-key-mode t)
        '';
      };

      zenburn-theme = {
        enable = true;
        config = ''
          ;; use variable-pitch fonts for some headings and titles
          (setq zenburn-use-variable-pitch t)

          ;; scale headings in org-mode
          (setq zenburn-scale-org-headlines t)

          ;; scale headings in outline-mode
          (setq zenburn-scale-outline-headlines t)

          (load-theme 'zenburn t)
        '';
      };
    };
  };
}
