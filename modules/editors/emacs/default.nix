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

    };
  };
}
