{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.herdr;

  appFiles = [
    "keys.json"
    "settings.json"
    "layouts/glass.json"
    "themes/rose-pine.json"
    "themes/rose-pine-dawn.json"
    "mods/leader/main.js"
    "mods/leader/mod.json"
    "mods/pluck/main.js"
    "mods/pluck/mod.json"
    "mods/copy-mode/main.js"
    "mods/copy-mode/mod.json"
    "mods/agent-badge/main.js"
    "mods/agent-badge/mod.json"
    "mods/beacons/main.js"
    "mods/beacons/mod.json"
    "mods/beacons/flame.svg"
    "mods/beacons/core.svg"
    "mods/beacons/brazier.svg"
    "mods/mission-control/main.js"
    "mods/mission-control/mod.json"
    "mods/branches/main.js"
    "mods/branches/mod.json"
    "mods/beacon-spaces/main.js"
    "mods/beacon-spaces/mod.json"
    "mods/beacon-spaces/diamond.svg"
    "mods/beacon-spaces/smoke.svg"
    "mods/beacon-agents/main.js"
    "mods/beacon-agents/mod.json"
    "mods/beacon-agents/diamond.svg"
    "mods/beacon-agents/flame.svg"
    "mods/beacon-agents/smoke.svg"
    "mods/beacon-prs/main.js"
    "mods/beacon-prs/mod.json"
    "mods/beacon-prs/diamond.svg"
    "mods/beacon-prs/sort-priority.svg"
    "mods/beacon-prs/sort-newest.svg"
    "mods/beacon-prs/sort-oldest.svg"
    "mods/toast-top/main.js"
    "mods/toast-top/mod.json"
  ];
in
{

  options.herdr = {
    enable = lib.mkEnableOption "enable herdr";

    configPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.config/dotfiles/modules/shell/herdr/config.toml";
      description = "Absolute path to the live config.toml symlinked into ~/.config/herdr.";
    };

    app = {
      enable = lib.mkEnableOption "Herdr desktop app config (mods, themes, keys, settings)";

      configDir = lib.mkOption {
        type = lib.types.str;
        default = "${config.home.homeDirectory}/.config/dotfiles/modules/shell/herdr/app";
        description = "Absolute path to the live app config files symlinked into ~/.config/herdr-app.";
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {

      verify.checks = [
        {
          type = "command";
          name = "herdr";
          desc = "Agent multiplexer";
        }
        {
          type = "file";
          path = "~/.config/herdr/config.toml";
          desc = "herdr config";
        }
      ];

      home.packages = with pkgs; [
        herdr
      ];

      xdg.configFile."herdr/config.toml".source = config.lib.file.mkOutOfStoreSymlink cfg.configPath;
    })

    (lib.mkIf cfg.app.enable {

      verify.checks = [
        {
          type = "command";
          name = "herdr-app";
          desc = "Herdr desktop app";
        }
        {
          type = "file";
          path = "~/.config/herdr-app/keys.json";
          desc = "herdr-app keys";
        }
        {
          type = "file";
          path = "~/.config/herdr-app/mods/leader/main.js";
          desc = "herdr-app leader mod";
        }
      ];

      xdg.configFile = lib.listToAttrs (
        map (file: {
          name = "herdr-app/${file}";
          value = {
            source = config.lib.file.mkOutOfStoreSymlink "${cfg.app.configDir}/${file}";
            force = true;
          };
        }) appFiles
      );

      home.activation.herdrAppRemoveUnlinkedFiles =
        lib.hm.dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ]
          ''
            for target in ${
              lib.escapeShellArgs (map (file: "${config.xdg.configHome}/herdr-app/${file}") appFiles)
            }; do
              if [[ -e "$target" && ! -L "$target" ]]; then
                run rm $VERBOSE_ARG "$target"
              fi
            done
          '';
    })
  ];
}
