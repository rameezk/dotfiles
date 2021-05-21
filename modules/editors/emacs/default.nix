{ pkgs, ... }: {

  programs.emacs = {
    enable = true;

    package = pkgs.emacsGcc;

    init = { enable = true; };
  };
}
