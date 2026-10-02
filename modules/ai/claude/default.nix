{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.ai.claude;
  jq = lib.getExe pkgs.jq;
in
{
  options.ai.claude = {
    enable = lib.mkEnableOption "Claude Code managed user settings merged into ~/.claude/settings.json";

    managedSettings = lib.mkOption {
      type = lib.types.path;
      default = ./settings.json;
      description = "JSON file whose keys are deep-merged over ~/.claude/settings.json on activation. Keys not listed here stay under Claude Code's control.";
    };
  };

  config = lib.mkIf cfg.enable {
    verify.checks = [
      {
        type = "file";
        path = "~/.claude/settings.json";
        desc = "claude code user settings";
      }
    ];

    home.file.".claude/keybindings.json".source = ./keybindings.json;

    home.activation.claudeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      claudeSettings="$HOME/.claude/settings.json"
      claudeSettingsCurrent='{}'
      if [ -s "$claudeSettings" ]; then
        claudeSettingsCurrent="$(cat "$claudeSettings")"
      fi
      mkdir -p "$HOME/.claude"
      claudeSettingsTmp="$(mktemp "$HOME/.claude/.settings.json.XXXXXX")"
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
  };
}
