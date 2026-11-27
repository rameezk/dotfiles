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
    verify.checks =
      map (dir: {
        type = "file";
        path = "~/${dir}/CLAUDE.md";
        desc = "global agent instructions (claude ${dir})";
      }) config.ai.claude.configDirs
      ++ [
        {
          type = "file";
          path = "~/.codex/AGENTS.md";
          desc = "global agent instructions (codex)";
        }
        {
          type = "file";
          path = "~/.pi/agent/AGENTS.md";
          desc = "global agent instructions (pi)";
        }
      ];

    home.file =
      lib.genAttrs (map (dir: "${dir}/CLAUDE.md") config.ai.claude.configDirs) (_: {
        source = config.lib.file.mkOutOfStoreSymlink cfg.instructionsPath;
      })
      // {
        ".codex/AGENTS.md".source = config.lib.file.mkOutOfStoreSymlink cfg.instructionsPath;
        ".pi/agent/AGENTS.md".source = config.lib.file.mkOutOfStoreSymlink cfg.instructionsPath;
      };
  };
}
