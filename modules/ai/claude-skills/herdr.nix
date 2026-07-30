{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.ai.claude-skills.herdr;
in
{
  options.ai.claude-skills.herdr = {
    enable = lib.mkEnableOption "Claude Code herdr skill (from herdrdev/herdr)";
  };

  config = lib.mkIf cfg.enable {
    home.file.".claude/skills/herdr".source = "${inputs.herdr-skill}/skills/herdr";

    verify.checks = [
      {
        type = "file";
        path = "~/.claude/skills/herdr/SKILL.md";
        desc = "claude herdr skill files";
      }
      {
        type = "command";
        name = "herdr";
        desc = "herdr CLI (claude herdr skill — enable herdr module)";
      }
    ];
  };
}
