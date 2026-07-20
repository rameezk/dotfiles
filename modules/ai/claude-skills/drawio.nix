{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.ai.claude-skills.drawio;
in
{
  options.ai.claude-skills.drawio = {
    enable = lib.mkEnableOption "Claude Code drawio skill (from jgraph/drawio-mcp)";
  };

  config = lib.mkIf cfg.enable {
    home.file.".claude/skills/drawio".source =
      "${inputs.drawio-skill}/plugins/claude-code/skills/drawio";

    verify.checks = [
      {
        type = "file";
        path = "~/.claude/skills/drawio/SKILL.md";
        desc = "claude drawio skill files";
      }
      {
        type = "file";
        path = "/Applications/draw.io.app/Contents/MacOS/draw.io";
        desc = "draw.io Desktop (claude drawio skill — install drawio cask)";
      }
    ];
  };
}
