{ pkgs, ... }: {

  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtk;
  };

  # Write init.el to the Nix store and symlink it to ~/.emacs.d/init.el
  #  This seems counter-intuitive, but I've had issues with nur/emacs-init previously
  home.file.".emacs.d/init.el".text = builtins.readFile ./config/init.el;

  home.packages = with pkgs; [
    gcc # needed to compile sqlite for org-roam
    sqlite # needed for org-roam

    # spell checking
    aspell
    aspellDicts.en # includes en_GB variant
    aspellDicts.en-computers
    aspellDicts.en-science

    # markdown
    multimarkdown

    # gpg
    pinentry-emacs
  ];
}
