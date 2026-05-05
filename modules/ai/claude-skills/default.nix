{ lib, ... }:
{
  imports = [
    ./docx.nix
    ./drawio.nix
  ];

  options.ai.claude-skills = {
    enable = lib.mkEnableOption "install Claude Code skills from anthropics/skills";
  };
}
