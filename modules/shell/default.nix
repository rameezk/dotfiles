{ pkgs, ... }: {

  imports = [ ./fish ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    stdlib = ''
      layout_poetry() {
        if [[ ! -f pyproject.toml ]]; then
            log_error 'No pyproject.toml found. Use `poetry new` or `poetry init` to create one first.'
            exit 2
        fi

        local VENV=$(poetry env list --full-path | cut -d' ' -f1)
        if [[ -z $VENV || ! -d $VENV/bin ]]; then
            poetry install
            local VENV=$(poetry env list --full-path | cut -d' ' -f1)
        fi

        export VIRTUAL_ENV=$VENV
        export POETRY_ACTIVE=1
        PATH_add "$VENV/bin"
      }
    '';
  };

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
    watch # watch and refresh commands
  ];
}
