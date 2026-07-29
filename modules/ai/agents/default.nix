{
  lib,
  config,
  ...
}:
let
  cfg = config.ai.agents;
in
{
  options.ai.agents = {
    enable = lib.mkEnableOption "shared global agent instructions symlinked into each agent's config directory";

    instructionsPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.config/dotfiles/modules/ai/agents/AGENTS.md";
      description = "Absolute path to the live AGENTS.md symlinked into each agent's config directory.";
    };
  };

  config = lib.mkIf cfg.enable {
    verify.checks = [
      {
        type = "file";
        path = "~/.claude/CLAUDE.md";
        desc = "global agent instructions (claude)";
      }
      {
        type = "file";
        path = "~/.codex/AGENTS.md";
        desc = "global agent instructions (codex)";
      }
    ];

    home.file.".claude/CLAUDE.md".source = config.lib.file.mkOutOfStoreSymlink cfg.instructionsPath;
    home.file.".codex/AGENTS.md".source = config.lib.file.mkOutOfStoreSymlink cfg.instructionsPath;
  };
}
