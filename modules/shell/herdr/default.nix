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
    })
  ];
}
