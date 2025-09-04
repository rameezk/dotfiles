{
  pkgs,
  lib,
  config,
  ...
}:
{

  imports = [
    ./fish
    ./tmux
    ./wezterm
    ./starship
  ];

  options = {
    shell.enable = lib.mkEnableOption "enable shell";
  };

  config = lib.mkIf config.shell.enable {

    fish.enable = true;
    tmux.enable = true;
    terminal.wezterm.enable = true;
    prompt.starship.enable = true;

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

        layout_pdm() {
          if [ ! -f "pyproject.toml" ]; then
            log_status "No pyproject.toml found. Execute \`pmd init\` to create a \`$PYPROJECT_TOML\` first."
          fi

          VIRTUAL_ENV=$(pdm venv list | grep "^\*"  | awk -F" " '{print $3}')

          if [ -z "$VIRTUAL_ENV" ] || [ ! -d "$VIRTUAL_ENV" ]; then
            log_status "No virtual environment exists. Executing \`pdm info\` to create one."
            pdm info
            VIRTUAL_ENV=$(pdm venv list | grep "^\*"  | awk -F" " '{print $3}')
          fi

          PATH_add "$VIRTUAL_ENV/bin"
          export PDM_ACTIVE=1
          export VIRTUAL_ENV
        }
      '';
    };

    programs.fzf = {
      enable = true;
      enableFishIntegration = true;
    };

    home.file.".ripgreprc".text = ''
      --hidden
      --glob=!.git/*
      --max-columns=150
      --max-columns-preview
      --smart-case
      --colors=line:none
      --colors=line:style:bold
    '';

    programs.bat = {
      enable = true;
      config = {
        # theme = "gruvbox-dark";
        pager = "less -FR";
      };
    };

    programs.eza = {
      enable = true;
      icons = "auto";
      git = true;
    };

    programs.zoxide = {
      enable = true;
      enableFishIntegration = true;
    };

    home.packages = with pkgs; [
      ripgrep # faster grepping
      jq # parsing JSON
      entr # file watching
      htop # process manager
      fd # a faster find
      shellcheck # linting shell scripts
      watch # watch and refresh commands
    ];
  };
}
