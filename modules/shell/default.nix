{ pkgs, ... }: {

  imports = [ ./fish ];

  home.packages = with pkgs; [
    ripgrep # faster grepping
    tmux # terminal multiplexor
    jq # parsing JSON
    entr # file watching
    fzf # fuzzy file finder
    htop # process manager
    neofetch # nice system info viewer
    fd # a faster find
    shellcheck # linting shell scripts
  ];
}
