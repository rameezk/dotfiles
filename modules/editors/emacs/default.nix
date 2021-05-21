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

      org = { enable = true; };

      org-roam = {
        enable = true;
        hook = [ "(after-init . org-roam-mode)" ];
        config = ''
          (setq org-roam-directory "~/Dropbox/DigitalGarden")
          (setq org-roam-graph-exclude-matcher '("inbox"))
        '';
      };
    };
  };
}
