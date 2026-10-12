{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
let
  cfg = config.ai.pi;
  piPkg = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
  jq = lib.getExe pkgs.jq;
in
{
  options.ai.pi = {
    enable = lib.mkEnableOption "pi coding agent (pi.dev)";

    managedSettings = lib.mkOption {
      type = lib.types.path;
      default = ./settings.json;
      description = "JSON file whose keys are merged over ~/.pi/agent/settings.json on activation. Top-level keys listed here (e.g. the pinned `packages` array) replace pi's value; keys not listed stay under pi's control.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ piPkg ];

    verify.checks = [
      {
        type = "file";
        path = "~/.pi/agent/settings.json";
        desc = "pi user settings";
      }
    ];

    home.activation.piSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      piSettings="$HOME/.pi/agent/settings.json"
      piSettingsCurrent='{}'
      if [ -s "$piSettings" ]; then
        piSettingsCurrent="$(cat "$piSettings")"
      fi
      mkdir -p "$HOME/.pi/agent"
      piSettingsTmp="$(mktemp "$HOME/.pi/agent/.settings.json.XXXXXX")"
      if printf '%s' "$piSettingsCurrent" | ${jq} -s '.[0] * .[1]' - ${cfg.managedSettings} > "$piSettingsTmp"; then
        if ! cmp -s "$piSettingsTmp" "$piSettings"; then
          run chmod 644 "$piSettingsTmp"
          run mv "$piSettingsTmp" "$piSettings"
        fi
      else
        warnEcho "Could not merge managed pi settings into $piSettings; leaving it unchanged"
      fi
      rm -f "$piSettingsTmp"
    '';
  };
}
