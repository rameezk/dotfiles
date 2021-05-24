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

      ;; start server
      (load "server")
      (unless (server-running-p) (server-start))
    '';

    usePackage = {
      consult = { enable = true; };

      evil = {
        enable = true;
        config = ''
          (evil-mode t)
        '';
      };

      general = {
        enable = true;
        config = builtins.readFile ./general-keybindings.el;
      };

      nix-mode = {
        enable = true;
        mode = [ ''"\\.nix\\'"'' ];
        # after = [ "company" ];
        extraPackages = [
          # pkgs.rnix-lsp
          pkgs.nixpkgs-fmt
        ];
        config = ''
          (setq nix-nixfmt-bin "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt")
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

      selectrum = {
        enable = true;
        config = ''
          (selectrum-mode +1)
        '';
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
