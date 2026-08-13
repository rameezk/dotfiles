{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.ai.claude-skills.docx;

  docxNodeModules = pkgs.buildNpmPackage {
    pname = "claude-docx-skill-deps";
    version = "0.0.0";
    src = ./docx-npm;
    npmDepsHash = "sha256-9ApfMPHtkdie+tu99tjbZs0yeQM52faF47dvdM80Jh0=";
    dontNpmBuild = true;
    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib/node_modules
      cp -r node_modules/. $out/lib/node_modules/
      runHook postInstall
    '';
  };
in
{
  options.ai.claude-skills.docx = {
    enable = lib.mkEnableOption "Claude Code docx skill (deps + skill files)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pandoc
      poppler-utils
    ];

    home.sessionVariables.NODE_PATH = "${docxNodeModules}/lib/node_modules";

    language.python.extraPackages = [
      (ps: [ ps.defusedxml ])
    ];

    home.file.".claude/skills/docx".source = "${inputs.claude-skills}/skills/docx";

    verify.checks = [
      {
        type = "command";
        name = "pandoc";
        desc = "pandoc (claude docx skill)";
      }
      {
        type = "command";
        name = "pdftoppm";
        desc = "pdftoppm from poppler-utils (claude docx skill)";
      }
      {
        type = "command";
        name = "soffice";
        desc = "LibreOffice soffice (claude docx skill — install libreoffice cask)";
      }
      {
        type = "file";
        path = "~/.claude/skills/docx/SKILL.md";
        desc = "claude docx skill files";
      }
    ];
  };
}
