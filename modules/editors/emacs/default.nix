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
