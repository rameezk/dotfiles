{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.vcs.hunk;

  tomlFormat = pkgs.formats.toml { };

  mkConfig = name: theme: tomlFormat.generate name (cfg.settings // { inherit theme; });

  followsAppearance = config.theme.active.enable && config.theme.active.followAppearance;
in
{

  options.vcs.hunk = {
    enable = lib.mkEnableOption "enable hunk";

    settings = lib.mkOption {
      type = tomlFormat.type;
      default = { };
      description = "Preferences written to hunk's config.toml, excluding the theme.";
    };

    darkTheme = lib.mkOption {
      type = lib.types.str;
      default = "github-dark-default";
      description = "Hunk theme id used when the OS appearance is dark.";
    };

    lightTheme = lib.mkOption {
      type = lib.types.str;
      default = "github-light-default";
      description = "Hunk theme id used when the OS appearance is light.";
    };
  };

  config = lib.mkIf cfg.enable {

    verify.checks = [
      {
        type = "command";
        name = "hunk";
        desc = "Terminal diff viewer for agentic changesets";
      }
      {
        type = "file";
        path = "~/.config/hunk/config.toml";
        desc = "hunk config";
      }
    ];

    home.packages = with pkgs; [
      hunk
    ];

    xdg.configFile =
      if followsAppearance then
        {
          "hunk/config-dark.toml".source = mkConfig "hunk-config-dark.toml" cfg.darkTheme;
          "hunk/config-light.toml".source = mkConfig "hunk-config-light.toml" cfg.lightTheme;
        }
      else
        {
          "hunk/config.toml".source = mkConfig "hunk-config.toml" cfg.darkTheme;
        };
  };
}
