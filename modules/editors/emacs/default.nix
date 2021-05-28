{ pkgs, ... }: {

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
  };

  # Write init.el to the Nix store and symlink it to ~/.emacs.d/init.el
  #  This seems counter-intuitive, but I've had issues with nur/emacs-init previously
  home.file.".emacs.d/init.el".text = builtins.readFile ./config/init.el;

  home.packages = with pkgs; [
    sqlite # needed for org-roam

    # spell checkiing
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # markdown
    multimarkdown
  ];
}
