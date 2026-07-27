{ lib, ... }:
{
  imports = [
    ./catppuccin
    ./rose-pine
  ];

  options.theme.active = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether any theme backend is active.";
    };

    darkFlavour = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Flavour used when the OS appearance is dark.";
    };

    lightFlavour = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Flavour used when the OS appearance is light.";
    };

    followAppearance = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether the active backend follows the OS light/dark appearance at runtime.";
    };

    syncPackage = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "Script that repoints theme symlinks and rethemes tmux.";
    };

    tmuxConfig = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Backend specific tmux status bar configuration.";
    };

    starshipLightSettings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Starship settings overlaid onto the light variant of the prompt.";
    };

    weztermScheme = {
      dark = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "WezTerm colour scheme used when the OS appearance is dark.";
      };

      light = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "WezTerm colour scheme used when the OS appearance is light.";
      };
    };
  };
}
