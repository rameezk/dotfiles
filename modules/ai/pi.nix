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
in
{
  options.ai.pi = {
    enable = lib.mkEnableOption "pi coding agent (pi.dev)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ piPkg ];
  };
}
