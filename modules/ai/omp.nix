{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
let
  cfg = config.ai.omp;
  ompPkg = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.omp;
in
{
  options.ai.omp = {
    enable = lib.mkEnableOption "omp coding agent (omp.sh)";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ ompPkg ];
  };
}
