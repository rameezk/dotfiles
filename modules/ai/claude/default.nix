{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.ai.claude;
  jq = lib.getExe pkgs.jq;

  mergeSettings = dir: ''
    claudeDir="$HOME/${dir}"
    claudeSettings="$claudeDir/settings.json"
    claudeSettingsCurrent='{}'
    if [ -s "$claudeSettings" ]; then
      claudeSettingsCurrent="$(cat "$claudeSettings")"
    fi
    mkdir -p "$claudeDir"
    claudeSettingsTmp="$(mktemp "$claudeDir/.settings.json.XXXXXX")"
    if printf '%s' "$claudeSettingsCurrent" | ${jq} -s '.[0] * .[1]' - ${cfg.managedSettings} > "$claudeSettingsTmp"; then
      if ! cmp -s "$claudeSettingsTmp" "$claudeSettings"; then
        run chmod 644 "$claudeSettingsTmp"
        run mv "$claudeSettingsTmp" "$claudeSettings"
      fi
    else
      warnEcho "Could not merge managed Claude settings into $claudeSettings; leaving it unchanged"
    fi
    rm -f "$claudeSettingsTmp"
  '';
in
{
  options.ai.claude = {
    enable = lib.mkEnableOption "Claude Code managed user settings merged into each Claude config directory's settings.json";

    configDirs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ".claude" ];
      example = [
        ".claude"
        ".claude-work"
      ];
      description = "Claude Code config directories, relative to $HOME, that receive the managed settings, keybindings, instructions and skills. Extra entries are used via CLAUDE_CONFIG_DIR.";
    };

    managedSettings = lib.mkOption {
      type = lib.types.path;
      default = ./settings.json;
      description = "JSON file whose keys are deep-merged over each config directory's settings.json on activation. Keys not listed here stay under Claude Code's control.";
    };
  };

  config = lib.mkIf cfg.enable {
    verify.checks = map (dir: {
      type = "file";
      path = "~/${dir}/settings.json";
      desc = "claude code user settings (${dir})";
    }) cfg.configDirs;

    home.file = lib.mkMerge [
      (lib.genAttrs (map (dir: "${dir}/keybindings.json") cfg.configDirs) (_: {
        source = ./keybindings.json;
      }))
      {
        ".claude/hooks/herdr-subagents.sh" = {
          source = ./herdr-subagents.sh;
          force = true;
        };

        ".claude/statusline-command.sh" = {
          source = ./statusline-command.sh;
          force = true;
        };
      }
    ];

    home.activation.claudeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] (
      lib.concatMapStrings mergeSettings cfg.configDirs
    );
  };
}
