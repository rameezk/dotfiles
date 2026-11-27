{
  lib,
  config,
  ...
}:
let
  cfg = config.ai.claude-skills.mermaid;
in
{
  options.ai.claude-skills.mermaid = {
    enable = lib.mkEnableOption "Claude Code mermaid skill (renders .mmd via nix run nixpkgs#mermaid-cli)";
  };

  config = lib.mkIf cfg.enable {
    home.file = lib.genAttrs (map (dir: "${dir}/skills/mermaid") config.ai.claude.configDirs) (_: {
      source = ./mermaid;
    });

    verify.checks = [
      {
        type = "file";
        path = "~/.claude/skills/mermaid/SKILL.md";
        desc = "claude mermaid skill files";
      }
    ];
  };
}
